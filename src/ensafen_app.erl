-module(ensafen_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  Dispatch = cowboy_router:compile([
    {'_', [{'_', safify_handler, []}]}
  ]),
  {ok, ListenAddress} = application:get_env(ensafen, listenAddress),
  {ok, ListenPort} = application:get_env(ensafen, listenPort),

  cowboy:start_http(my_http_listener, 100,
      [
        {ip, ListenAddress},
        {port, ListenPort}
      ],
    [{env, [{dispatch, Dispatch}]}]
  ),
	ensafen_sup:start_link().

stop(_State) ->
	ok.
