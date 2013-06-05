-module(xml).

-import(xmerl).

-export([game_as_xml/1, turn_as_xml/1, id_as_xml/1, result_as_xml/1]).

-include("../include/game.hrl").

game_as_xml(Game) ->
    SimpleContent = game_as_simple_content(Game),
    lists:flatten(xmerl:export_simple(SimpleContent, xmerl_xml)).

game_as_simple_content(Game) ->
	[{game,
	  [{id, [integer_to_list(Game#game.id)]},
	   {description, [Game#game.description]},
	   {x_player, [Game#game.x_player]},
	   {o_player, [Game#game.o_player]},
	   {status, [Game#game.status]}]
    }].

turn_as_xml(Turn) ->
    SimpleContent = turn_as_simple_content(Turn),
    lists:flatten(xmerl:export_simple(SimpleContent, xmerl_xml)).

turn_as_simple_content(Turn) ->
	[{turn,
	  [{id, [integer_to_list(Turn#turn.id)]},
	   {game_id, [integer_to_list(Turn#turn.game_id)]},
	   {player, [Turn#turn.player]},
	   {column, [integer_to_list(Turn#turn.column)]},
	   {row, [integer_to_list(Turn#turn.row)]}]
    }].

id_as_xml(Id) ->
    SimpleContent = id_as_simple_content(Id),
    lists:flatten(xmerl:export_simple(SimpleContent, xmerl_xml)).

id_as_simple_content(Id) ->
	[{id, [integer_to_list(Id)]}].

result_as_xml(Result) ->
    SimpleContent = result_as_simple_content(Result),
    lists:flatten(xmerl:export_simple(SimpleContent, xmerl_xml)).

result_as_simple_content(Result) ->
	[{result, [Result]}].
