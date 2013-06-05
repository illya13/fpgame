-module(db).

-export([init/0, create_game/1, get_game/1, 
		 get_all_games/0, get_all_not_started_games/0,
		 update_game_x_player/2, update_game_o_player/2,
		 start_game/2, finish_game/1,
		 add_game_turn/4, get_turn/1, get_all_game_turns/1]).


-include("../include/game.hrl").

%% status: '0'-not started, '1'-finished, 'x'-started by x, 'o'-started by o

init() -> init([node()]).
	
init(NodeList) ->
	mnesia:create_schema(NodeList),
	mnesia:start(),
	
	mnesia:create_table(unique, 
						[{attributes, record_info(fields, unique)},
						 {disc_copies, NodeList}] ),
	
	mnesia:create_table(game,
						[{attributes, record_info(fields, game)},
						 {disc_copies, NodeList}]),
	
	mnesia:create_table(turn,
						[{attributes, record_info(fields, turn)},
						 {disc_copies, NodeList}]),

	mnesia:wait_for_tables([unique, game, turn], 10000).


create_game(Description) ->
	Id = mnesia:dirty_update_counter(unique, game, 1),
	Game = #game{id=Id, description=Description, status="0"},
	F = fun() ->
				mnesia:write(Game)
		end,
	{atomic, _} = mnesia:transaction(F),
	Id.

get_game(Id) ->
	F = fun() ->
				mnesia:read(game, Id, read)
		end,
	{atomic, [Head|_]} = mnesia:transaction(F),
	Head.

update_game_x_player(Id, Player) ->
	F = fun() -> 
				[P] = mnesia:wread({game, Id}),
				mnesia:write(game, P#game{x_player=Player}, write)
		end,
	{atomic, Value} = mnesia:transaction(F),
	Value.

update_game_o_player(Id, Player) ->
	F = fun() -> 
				[P] = mnesia:wread({game, Id}),
				mnesia:write(game, P#game{o_player=Player}, write)
		end,
	{atomic, Value} = mnesia:transaction(F),
	Value.

start_game(Id, Player) ->
	F = fun() -> 
				[P] = mnesia:wread({game, Id}),
				mnesia:write(game, P#game{status=Player}, write)
		end,
	{atomic, Value} = mnesia:transaction(F),
	Value.

finish_game(Id) ->
	F = fun() -> 
				[P] = mnesia:wread({game, Id}),
				mnesia:write(game, P#game{status="1"}, write)
		end,
	{atomic, Value} = mnesia:transaction(F),
	Value.

get_all_games() ->
	F = fun() ->
                Game = #game{id = '$1', _ = '_'},
                mnesia:select(game, [{Game, [], ['$1']}])
        end,
	{atomic, Value} = mnesia:transaction(F),
	Value.

get_all_not_started_games() ->
	F = fun() ->
                Game = #game{id = '$1', status="0", _ = '_'},
                mnesia:select(game, [{Game, [], ['$1']}])
        end,
	{atomic, Value} = mnesia:transaction(F),
	Value.

add_game_turn(GameId, Player, Column, Row) ->
	Id = mnesia:dirty_update_counter(unique, turn, 1),
	Turn = #turn{id=Id, game_id=GameId, player=Player, column=Column, row=Row},
	F = fun() ->
				mnesia:write(Turn)
		end,
	{atomic, _} = mnesia:transaction(F),
	Id.

get_turn(Id) ->
	F = fun() ->
				mnesia:read(turn, Id, read)
		end,
	{atomic, [Head|_]} = mnesia:transaction(F),
	Head. 

get_all_game_turns(GameId) ->
	F = fun() ->
                Turn = #turn{id = '$1', game_id=GameId, _ = '_'},
                mnesia:select(turn, [{Turn, [], ['$1']}])
        end,
	{atomic, Value} = mnesia:transaction(F),
	Value.

	