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
  % We need to know the requested path, so we can
  % forward the request onward.
  % We also need to know the request method and
  % any query parameters or request body.

  ReqList = cowboy_req:to_list(Req),

  [ReqBody] = [X || {buffer, X} <- ReqList],
  [ReqHeaders] = [X || {headers, X} <- ReqList],
  [ReqPath] = [X || {path, X} <- ReqList],
  [ReqMethod] = [X || {method, X} <- ReqList],
  [Query] = [X || {qs, X} <- ReqList],

  io:format("Req: ~p~nMethod: ~p~nReqPath: ~p~nQuery:~p~nHeaders: ~p~nBody: ~p~n",
    [Req, ReqMethod, ReqPath, Query, ReqHeaders, ReqBody]),

  {ok, Destination} = application:get_env(ensafen, destination),

  {ok, DestinationUri} = http_uri:parse(http_uri:decode(Destination)),
  io:format("Destination: ~p~n", [Destination]),
  io:format("Destination URI: ~p~n", [DestinationUri]),

  {_Scheme, _UserInfo, DestinationHost, DestinationPort, DestinationPath,
    _Query} = DestinationUri,

  io:format("DestinationHost: ~p~n", [DestinationHost]),
  io:format("DestinationPort: ~p~n", [DestinationPort]),
  % io:format("DestinationPath: ~p~n", [DestinationPath]),

  % Our reverse proxy destination may be specified as a URI, and as such
  % it may have a path component. We will prepend that to the requested path
  % before submitting our request.
  % The destination path may end with a slash, so we remove it before
  % concatenating with the request path.
  % We will also concatenate here the query string supplied in the request,
  % if any.
  TargetPath = string:strip(DestinationPath, right, $/) ++
    binary_to_list(ReqPath) ++ case binary_to_list(Query) of
      "" -> "";
      QueryString -> "?" ++ QueryString
    end,
  io:format("TargetPath: ~p~n", [TargetPath]),

  % For the most part we are passing the request headers intact.
  % However, we need to replace the host field with the destination host (and
  % port).

  % First remove the existing host field from the request header
  % Then add the new host field
  DestinationHostTuple = {<<"host">>,
    list_to_binary(DestinationHost ++ ":" ++ integer_to_list(DestinationPort))},
  NewReqHeaders = [{Field, Value} || {Field, Value} <- ReqHeaders,
    Field /= <<"host">>] ++ [DestinationHostTuple],
  io:format("NewReqHeaders: ~p~n", [NewReqHeaders]),

  % Here we just open a connection to the target host...

  {ok, ConnPid} = gun:open(DestinationHost, DestinationPort),
  io:format("ConnPid: ~p~n", [ConnPid]),

  % ... and wait for it to be established

  {ok, ConnProtocol} = gun:await_up(ConnPid),
  io:format("ConnProtocol: ~p~n", [ConnProtocol]),

  % Now we pass the request to the Destination host.
  % Note that to do this properly, we should also preserve the headers,
  % except for the Host field.

  StreamRef = case ReqMethod of
    <<"GET">> ->
      gun:get(ConnPid, TargetPath, NewReqHeaders);
    <<"HEAD">> ->
      gun:head(ConnPid, TargetPath, NewReqHeaders);
    <<"DELETE">> ->
      gun:delete(ConnPid, TargetPath, NewReqHeaders);

    % The below methods also need the request body
    <<"POST">> ->
      gun:post(ConnPid, TargetPath, NewReqHeaders, ReqBody);
    <<"PUT">> ->
      gun:put(ConnPid, TargetPath, NewReqHeaders, ReqBody);
    <<"PATCH">> ->
      gun:patch(ConnPid, TargetPath, NewReqHeaders, ReqBody);
    _ ->
      nil
  end,

  % Now we wait for the response, and forward it to the client.
  % An improvement here would be to transmit each chunk as we receive it,
  % in the case that the response has multiple chunks,
  % rather than waiting for the entire message before beginning our reply.
  case gun:await(ConnPid, StreamRef) of
    {response, fin, RspStatus, RspHeaders} ->
      {ok, Req2} = cowboy_req:reply(RspStatus, RspHeaders, <<>>, Req),
      {ok, Req2, State};
    {response, nofin, RspStatus, RspHeaders} ->
      io:format("RspStatus: ~p~n", [RspStatus]),
      io:format("RspHeaders: ~p~n", [RspHeaders]),
      {ok, RspBody} = gun:await_body(ConnPid, StreamRef),
      {ok, Req2} = cowboy_req:reply(RspStatus, RspHeaders, RspBody, Req),
      {ok, Req2, State}
  end.

terminate(_Reason, _Req, _State) ->
  ok.
