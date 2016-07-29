-module(ensafen_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  % Adding Cowboy here
  Dispatch = cowboy_router:compile([
    {'_', [{'_', safify_handler, []}]}
  ]),
  application:ensure_all_started(gun),
  cowboy:start_http(my_http_listener, 100, [{port, 8080}],
    [{env, [{dispatch, Dispatch}]}]
  ),
	ensafen_sup:start_link().

stop(_State) ->
	ok.
