-module(generator_slave).

-export([start/0, request/2, loop/0]).

% start slave and return {ok, Pid} when started
start() ->
  process_flag(trap_exit, true),
  Pid = spawn_link(generator_slave, loop, []),
  register(generator_slave, Pid),
  {ok, Pid}.

% make a request
request([],_) -> ok;

request(Request, Timeout) ->
  generator_slave ! {request, self(), Request},
  receive
    {result, Result} -> Result;
    {'EXIT', _Pid, Reason} -> {error, Reason}
  after Timeout -> timeout
  end.

loop() ->
  receive
    {request, Pid, Request} ->
      {ResponseMessage,Result} = execute_request(Request),
      Pid ! {result, {ResponseMessage,Result}}
  end,
  loop().

priv_execute_request({Method,{Url,Headers,_,_},HttpOptions,RequestOptions,_}) when Method == get;Method == delete ->
  httpc:request(Method,{Url,Headers},HttpOptions,RequestOptions);

priv_execute_request({Method,{Url,Headers,ContentType,Body},HttpOptions,RequestOptions,_}) when Method == post;Method == put ->
  httpc:request(Method,{Url,Headers,ContentType,Body},HttpOptions,RequestOptions).

execute_request({Method,{Url,Headers,ContentType,Body},HttpOptions,RequestOptions,Pid}) ->
  inets:start(),
  {ResponseCode, Result} = priv_execute_request({Method,{Url,Headers,ContentType,Body},HttpOptions,RequestOptions,Pid}),
  case ResponseCode of
    ok ->
      {{Version,Status,StatusMessage},ResponseHeaders,ResponseBody} = Result,
      {success,{
        {httpVersion,Version},
        {status,Status},
        {message,StatusMessage},
        {headers,ResponseHeaders},
        {response,ResponseBody}
      }};
    _ ->
      {ResponseCode, Result} 
  end.

