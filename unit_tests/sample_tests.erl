-module(sample_tests).
-include_lib("eunit/include/eunit.hrl").

-import(generator_slave, [execute_request/1]).
-export([sample/0]).

sample() ->
  {ok, RequestId} = execute_request(
    {
      get,
      {
        "http://localhost:1337",
        [
          {"Accept","Application/json"},
          {"Content-Type","Application/json"}
        ],
        "application/json",
        "{this: test}"
      },
      [
        {timeout,500},
        {connect_timeout,200}
      ],
      [{sync,false}],
      self()
    }),
  receive 
    {http,{RequestId, Result}} ->
      Result
  after
    500 -> 
      error
  end.
