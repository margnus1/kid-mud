-module(ws_test).

-export([handle_message/2]).

-include("yaws_api.hrl").

-export([relay/1]).

%% start of a fragmented message
handle_message(#ws_frame_info{fin=0,
                              opcode=FragType,
                              data=Data},
               {State, none, <<>>}) ->
    {noreply, {State, FragType, Data}};


%% non-final continuation of a fragmented message
handle_message(#ws_frame_info{fin=0,
                              data=Data,
                              opcode=continuation},
               {State, FragType, FragAcc}) ->
    {noreply, {State, FragType, <<FragAcc/binary,Data/binary>>}};


%% end of text fragmented message
handle_message(#ws_frame_info{fin=1,
                              opcode=continuation,
                              data=Data,
                              ws_state=Socket},
               {State, text, FragAcc}) ->
    Unfragged = <<FragAcc/binary, Data/binary>>,
    NewMessage = {text, Unfragged},
    case input(NewMessage, State, Socket) of
        {reply, Rep, RState} ->
            {reply, Rep, {RState, none, <<>>}};
        {noreply, RState} ->
            {noreply, {RState, none, <<>>}};
        {close, Reason} ->
            {close, Reason}
    end;

             

%% unfragmented text message
handle_message(#ws_frame_info{opcode=text, data=Data, ws_state=Socket},
               {State, none, <<>>}) ->
    NewMessage = {text, Data},
    case input(NewMessage, State, Socket) of
        {reply, Rep, RState} ->
            {reply, Rep, {RState, none, <<>>}};
        {noreply, RState} ->
            {noreply, {RState, none, <<>>}};
        {close, Reason} ->
            {close, Reason}
    end;


%% end of binary fragmented message
handle_message(#ws_frame_info{fin=1,
                              opcode=continuation,
                              data=Data,
                              ws_state=Socket},
               {State, binary, FragAcc}) ->
    Unfragged = <<FragAcc/binary, Data/binary>>,
    NewMessage = {binary, Unfragged},
    case input(NewMessage, State, Socket) of
        {reply, Rep, RState} ->
            {reply, Rep, {RState, none, <<>>}};
        {noreply, RState} ->
            {noreply, {RState, none, <<>>}};
        {close, Reason} ->
            {close, Reason}
    end;


handle_message(#ws_frame_info{opcode=binary,
                              data=Data,
                              ws_state=Socket},
               {State, none, <<>>}) ->
    NewMessage = {binary, Data},
    case input(NewMessage, State, Socket) of
        {reply, Rep, RState} ->
            {reply, Rep, {RState, none, <<>>}};
        {noreply, RState} ->
            {noreply, {RState, none, <<>>}};
        {close, Reason} ->
            {close, Reason}
    end;


handle_message(#ws_frame_info{opcode=ping,
                              data=Data,
                              ws_state=State},
                Acc) ->
    yaws_websockets:send(State, {pong, Data}),
    {noreply, Acc};


handle_message(#ws_frame_info{opcode=pong}, Acc) ->
    %% A response to an unsolicited pong frame is not expected.
    %% http://tools.ietf.org/html/\
    %%        draft-ietf-hybi-thewebsocketprotocol-08#section-4
    {noreply, Acc};


%% According to RFC 6455 section 5.4, control messages like close
%% MAY be injected in the middle of a fragmented message, which is
%% why we pass FragType and FragAcc along below. Whether any clients
%% actually do this in practice, I don't know.
handle_message(#ws_frame_info{opcode=close,
                              length=Len,
                              data=Data,
                              ws_state=Socket},
               {State, FragType, FragAcc}) ->
    NewMessage = case Len of
                     0 ->
                         {close, 1000, <<>>};
                     _ ->
                         <<Status:16/big, Msg/binary>> = Data,
                         {close, Status, Msg}
                 end,
    case input(NewMessage, State, Socket) of
        {reply, Rep, RState} ->
            {reply, Rep, {RState, FragType, FragAcc}};
        {noreply, RState} ->
            {noreply, {RState, FragType, FragAcc}};
        {close, Reason} ->
            {close, Reason}
    end;


handle_message(#ws_frame_info{}, Acc) ->
    {noreply, Acc}.

input({text, Name}, initial, Socket) ->
    Relay = spawn(?MODULE, relay, [Socket]),
    AsciiName = unicode:characters_to_list(Name),
    case playermaster:start_player(AsciiName, Relay) of
        {ok, Server} ->
            link(Relay),
            {reply, {text, <<"OK">>}, {Relay, Server}};
        login_failed ->
            exit(Relay, normal),
            {reply, {text, <<"ERR You cannot connect with that name">>}, initial};
        {login_failed, Reason} ->
            exit(Relay, normal),
            {reply, {text, unicode:characters_to_binary(
                             ["ERR ", Reason])}, initial}
    end;

input({text, <<"logout">>}, {Relay, Client}, _Socket) ->
    player:command(Client, "logout"),
    unlink(Relay), 
    exit(Relay, normal),
    {close, normal};

input({close, _, _}, {Relay, Client}, _Socket) ->
    player:command(Client, "logout"),
    unlink(Relay), 
    exit(Relay, normal),
    {close, normal};

input({text, String}, {Relay, Client}, _Socket) ->
    player:command(Client, unicode:characters_to_list(String)),
    {noreply, {Relay, Client}}.


relay(Socket) ->
    receive 
        {message, Message} ->
            yaws_websockets:send(
              Socket, {text, unicode:characters_to_binary(
                               ["LOG ", Message])});
        {status, Status} ->
            yaws_websockets:send(
              Socket, {text, unicode:characters_to_binary(
                               ["STATUS ", Status])})
    end,
    relay(Socket).
