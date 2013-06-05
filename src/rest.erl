-module(rest).

-export([init/0, 
		 create_game/3, get_game/3, start_game/3, finish_game/3]).

init() ->
	inets:start(),
	inets:start(httpd, 
				[{port, 8080},
				 {server_name, "localhost"},
				 {document_root, "."},
				 {modules, [mod_esi]},
				 {server_root, "."},
				 
				 {erl_script_alias, {"/ws", [rest, io]}},
				
				 {mime_types, 
				  [{"html", "text/html"},
				   {"xml", "text/xml"}]},

				 {modules,
				  [mod_alias,
				   mod_auth,
				   mod_esi,
				   mod_actions,
				   mod_cgi,
				   mod_get,
				   mod_head,
				   mod_dir,
				   mod_log,
				   mod_disk_log]}]).


create_game(SessionID, _Env, Input) ->
	mod_esi:deliver(SessionID, 
					[xml:id_as_xml(db:create_game(Input))]).

get_game(SessionID, _Env, Input) ->
	Id = list_to_integer(Input),
	mod_esi:deliver(SessionID, 
					[xml:game_as_xml(db:get_game(Id))]).

start_game(SessionID, _Env, Input) ->
	Id = list_to_integer(Input),
	mod_esi:deliver(SessionID, 
					[xml:result_as_xml(db:start_game(Id, "x"))]).

finish_game(SessionID, _Env, Input) ->
	Id = list_to_integer(Input),
	mod_esi:deliver(SessionID, 
					[xml:result_as_xml(db:finish_game(Id))]).
