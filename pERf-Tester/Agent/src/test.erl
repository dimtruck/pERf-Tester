%% Copyright
-module(test).
-author("dimi5963").

-export([call/1]).

%% API
call(Request) ->
  call_internal(Request),
  call_internal(Request).

call_internal({Method,{Url,Headers,_ContentType,_Body},HttpOptions,RequestOptions}) ->
  {ResponseCode, Result} = httpc:request(Method,{Url,Headers},HttpOptions,RequestOptions),
%%  case ResponseCode of
%%    ok ->
%%      {{Version,Status,StatusMessage},ResponseHeaders,ResponseBody} = Result,
%%      {success,{
%%        {httpVersion,Version},
%%        {status,Status},
%%        {message,StatusMessage},
%%        {headers,ResponseHeaders},
%%        {response,ResponseBody}
%%      }};
%%    _ ->
      {ResponseCode, Result}.
%%  end.