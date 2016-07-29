-module(safify_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
  % Here I've just printed out the Req tuple, and by inspection,
  % determined some fields of interest.
  % In particular we need to know the requested Route, so we can
  % forward the request onward.
  % We also need to know the request method and
  % any query parameters or POST data.
  {_, _, _, _, _, Method, _Protocol, 
   {_SourceIP, _SourcePort},
   _Host, _, _Port, Route,
   _, Query, _, _,
   _Headers,
   _, _, _, _,
   Body, _, _, _, _, _,
   _} = Req,

  DestinationHost = application:get_config(destinationHost),
  DestinationPort = application:get_config(destinationPort),

  % Here we just open a connection to the target host
  {ok, ConnPid} = gun:open(DestinationHost, DestinationPort),
  {ok, _ConnProtocol} = gun:await_up(ConnPid),

  % Now we pass the request to the Destination host.
  % Note that to do this properly, we should also preserve the headers,
  % except for the Host field.
  StreamRef = case Method of 
    <<"GET">> ->
      gun:get(ConnPid, Route, []);
    <<"HEAD">> ->
      gun:head(ConnPid, Route, []);
    <<"DELETE">> ->
      gun:delete(ConnPid, Route, []);

    % The below methods also need the request body
    <<"POST">> ->
      gun:post(ConnPid, Route, []);
    <<"PUT">> ->
      gun:put(ConnPid, Route, []);
    <<"PATCH">> ->
      gun:patch(ConnPid, Route, []);
    _ ->
      nil
  end,

  % Now we wait for, and capture, the response
  % An improvement here would be to transmit each chunk as we receive it,
  % in the case that the response has multiple chunks.

	{ok, Req2} = cowboy_req:reply(200,
    [{<<"content-type">>, <<"text/plain">>}],
    io_lib:format("Req: ~p~nMethod: ~p~nRoute: ~p~nQuery:~p~nBody: ~p~n",
      [Req, Method, Route, Query, Body]),
    Req),

	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.


% receive_data(ConnPid, MRef, StreamRef) ->
%   receive
%     {gun_data, ConnPid, StreamRef, nofin, Data} ->
%     io:format("~s~n", [Data]),
%     receive_data(ConnPid, MRef, StreamRef);
%     {gun_data, ConnPid, StreamRef, fin, Data} ->
%     io:format("~s~n", [Data]);
%     {'DOWN', MRef, process, ConnPid, Reason} ->
%     error_logger:error_msg("Oops!"),
%     exit(Reason)
%   after 1000 ->
%     exit(timeout)
% end.
% 
% print_body(ConnPid, MRef) ->
%   StreamRef = gun:get(ConnPid, "/"),
%   receive
%     {gun_response, ConnPid, StreamRef, fin, Status, Headers} ->
%     no_data;
%     {gun_response, ConnPid, StreamRef, nofin, Status, Headers} ->
%     receive_data(ConnPid, MRef, StreamRef);
%     {'DOWN', MRef, process, ConnPid, Reason} ->
%     error_logger:error_msg("Oops!"),
%     exit(Reason)
%   after 1000 ->
%     exit(timeout)
% end.
