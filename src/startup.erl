-module(startup).
-export([init/0]).

init() ->
	db:init(),
	rest:init().
