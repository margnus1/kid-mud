%% @type zone() -> #zone {id=integer(), exits=[exit()], npc=[npc()], desc=string()}
%% @type exit() -> {Direction :: north | east | south | west, Id :: integer() | none}
-record(zone, {id, exits=[{east, none}, {west, none}, {north, none}, {south, none}], npc=[], desc=""}).

