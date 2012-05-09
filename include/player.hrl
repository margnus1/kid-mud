%% @type player() = #player {name=string(), location=integer(), health={time=erlang=timestamp(), hp=integer()}}
-record(player, {name, location=0, health={erlang:now(), 100.0}}).
