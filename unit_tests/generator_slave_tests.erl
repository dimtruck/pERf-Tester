-module(generator_slave_tests).
-include_lib("eunit/include/eunit.hrl").

-import(generator_slave, [execute_request/1]).

setup_success_test_() ->
  { setup,
      fun setup/0,
      fun cleanup/1,
      {inorder,
        [
          fun () -> timer:sleep(3000) end,
          fun () -> success_sending_request_to_response([get,post,put,delete]) end,
          fun () -> failure_sending_request_to_response(400,[get,post,put,delete]) end,
          fun () -> failure_sending_request_to_response(401,[get,post,put,delete]) end,
          fun () -> failure_sending_request_to_response(403,[get,post,put,delete]) end,
          fun () -> failure_sending_request_to_response(404,[get,post,put,delete]) end,
          fun () -> failure_sending_request_to_response(500,[get,post,put,delete]) end,
          fun () -> failure_connecting() end
        ]
      }
  }.

setup() ->
  ?cmd("/home/hERmes/test_responder/responder.sh start").

cleanup(_) ->
  ?cmd("/home/hERmes/test_responder/responder.sh stop"). 


success_sending_request_to_response([]) -> ok;

success_sending_request_to_response([Head|Methods]) ->
  success_sending_request_to_response(Head),
  success_sending_request_to_response(Methods);

success_sending_request_to_response(Method) ->
  ?assertEqual(
      {success,
        {httpVersion,"HTTP/1.1"},
        {status,200},
        {message,"OK"},
        {headers,[
          {"connection","keep-alive"},
          {"content-length","13"},
          {"content-type","text/plain"},
          {"x-powered-by","Express"}
        ]},
        {response,"Home response"}
      },
  execute_request(
    {
      Method,
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
      [],
      self()
    })).

failure_connecting() ->
  ?assertEqual(
      {error,
        {failed_connect,
          [
            {to_address,
              {"localhost",1338}
            },
            {inet,
              [inet],
              econnrefused
            }
          ]
        }
     },
  execute_request(
    {
      get,
      {
        "http://localhost:1338",
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
      [],
      self()
    })).


failure_sending_request_to_response(StatusCode, [Head|Methods]) ->
  case StatusCode of
    400 ->
      failure_sending_request_to_response(bad_request,Head),
      failure_sending_request_to_response(StatusCode, Methods);
    401 ->
      failure_sending_request_to_response(unauthorized,Head),
      failure_sending_request_to_response(StatusCode, Methods);
    403 ->
      failure_sending_request_to_response(forbidden,Head),
      failure_sending_request_to_response(StatusCode, Methods);
    404 ->
      failure_sending_request_to_response(not_found,Head),
      failure_sending_request_to_response(StatusCode, Methods);
    500 ->
      failure_sending_request_to_response(internal_server_error,Head),
      failure_sending_request_to_response(StatusCode, Methods)
  end;

failure_sending_request_to_response(_,[]) -> [];

failure_sending_request_to_response(StatusMessage,Method) ->
  case StatusMessage of
    bad_request ->
      ?assertEqual(
        {success,
          {httpVersion,"HTTP/1.1"},
          {status,400},
          {message,"Bad Request"},
          {headers,[
            {"connection","keep-alive"},
            {"content-length","11"},
            {"content-type","text/plain"},
            {"x-powered-by","Express"}
          ]},
          {response,"bad request"}
        },
        execute_request(
        {
          Method,
          {
            "http://localhost:1337/bad_request",
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
          [],
          self()
        }));
    unauthorized ->
      ?assertEqual(
        {success,
          {httpVersion,"HTTP/1.1"},
          {status,401},
          {message,"Unauthorized"},
          {headers,[
            {"connection","keep-alive"},
            {"content-length","12"},
            {"content-type","text/plain"},
            {"x-powered-by","Express"}
          ]},
          {response,"unauthorized"}
        },
        execute_request(
        {
          Method,
          {
            "http://localhost:1337/unauthorized",
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
          [],
          self()
        }));
    forbidden ->
      ?assertEqual(
        {success,
          {httpVersion,"HTTP/1.1"},
          {status,403},
          {message,"Forbidden"},
          {headers,[
            {"connection","keep-alive"},
            {"content-length","9"},
            {"content-type","text/plain"},
            {"x-powered-by","Express"}
          ]},
          {response,"forbidden"}
        },
        execute_request(
        {
          Method,
          {
            "http://localhost:1337/forbidden",
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
          [],
          self()
        }));
    not_found ->
      case Method of
        get ->
          ?assertEqual(
            {success,
              {httpVersion,"HTTP/1.1"},
              {status,404},
              {message,"Not Found"},
              {headers,[
                {"connection","keep-alive"},
                {"content-length","21"},
                {"content-type","text/plain"},
                {"x-powered-by","Express"}
              ]},
              {response,"Cannot GET /not_found"}
            },
            execute_request(
            {
              Method,
              {
                "http://localhost:1337/not_found",
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
              [],
              self()
            }));
        post ->
          ?assertEqual(
            {success,
              {httpVersion,"HTTP/1.1"},
              {status,404},
              {message,"Not Found"},
              {headers,[
                {"connection","keep-alive"},
                {"content-length","22"},
                {"content-type","text/plain"},
                {"x-powered-by","Express"}
              ]},
              {response,"Cannot POST /not_found"}
            },
            execute_request(
            {
              Method,
              {
                "http://localhost:1337/not_found",
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
              [],
              self()
            }));
        put ->
          ?assertEqual(
            {success,
              {httpVersion,"HTTP/1.1"},
              {status,404},
              {message,"Not Found"},
              {headers,[
                {"connection","keep-alive"},
                {"content-length","21"},
                {"content-type","text/plain"},
                {"x-powered-by","Express"}
              ]},
              {response,"Cannot PUT /not_found"}
            },
            execute_request(
            {
              Method,
              {
                "http://localhost:1337/not_found",
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
              [],
              self()
            }));
        delete ->
          ?assertEqual(
            {success,
              {httpVersion,"HTTP/1.1"},
              {status,404},
              {message,"Not Found"},
              {headers,[
                {"connection","keep-alive"},
                {"content-length","24"},
                {"content-type","text/plain"},
                {"x-powered-by","Express"}
              ]},
              {response,"Cannot DELETE /not_found"}
            },
            execute_request(
            {
              Method,
              {
                "http://localhost:1337/not_found",
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
              [],
              self()
            }))

      end;
    internal_server_error ->
      ?assertEqual(
        {success,
          {httpVersion,"HTTP/1.1"},
          {status,500},
          {message,"Internal Server Error"},
          {headers,[
            {"connection","keep-alive"},
            {"content-length","21"},
            {"content-type","text/plain"},
            {"x-powered-by","Express"}
          ]},
          {response,"Internal Server Error"}
        },
        execute_request(
        {
          Method,
          {
            "http://localhost:1337/internal_server_error",
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
          [],
          self()
        }))
  end.
