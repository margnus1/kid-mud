%% @type player() = #player {name=string(), location=integer(), health={time=erlang=timestamp(), hp=integer()}}
-record(player, {name, location=0, health={time=erlang:now(), hp=100.0}}).
