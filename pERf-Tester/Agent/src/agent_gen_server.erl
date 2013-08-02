%% @doc This gen server implements client/server behavior - this is where we're going to execute requests to the browser
%% application:start(inets) must be called before this is executed.  Must be in the application definition

%% Copyright
-module(agent_gen_server).
-author("dimi5963").

-behaviour(gen_server).

%% abstract supervisor API
-export([start_link/0, stop/0]).

%% agent supervisor API
-export([concurrent_request/2]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%% API
%% starts up the request synchronously.  Uses the following call:
start_link() ->
  %% gen_server:start_link( ServerName, CallBackModule, Arguments, Options)
  %%  - ServerName - {local, Name} or {global, Name}
  %%  - CallbackModule - module where callbacks are placed
  %%  - Arguments - list of args passed into init function
  %%  - Options - list that allows to set fullsweep_after, heapsize, and other tracing and debugging flags
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% send the stop call
stop() ->
  gen_server:cast(?MODULE, stop).

%% initializes LoopBack of server
%% returns {ok, LoopData}
init(_Args) ->
  %% set up requests
  {ok, null}.

%% handle synchronous responses
handle_call({create_request,NoConcurrentRequests,{Method,{Url,Headers,_ContentType,_Body},HttpOptions,RequestOptions,_}}, _From, _LoopData) ->
  {ResponseCode, Reason} = httpc:request(Method,{Url,Headers},HttpOptions,RequestOptions),
  {reply, NoConcurrentRequests, {ResponseCode, Reason}}.

%% handle asynchronous responses
handle_cast(stop, LoopData) ->
  {stop, normal, LoopData};

handle_cast({create_request,_RequestNo, {Method,{Url,Headers,_ContentType,_Body},HttpOptions,RequestOptions}}, _LoopData) ->
  {ResponseCode, Reason} = httpc:request(Method,{Url,Headers},HttpOptions,RequestOptions),
  case ResponseCode of
    error ->
      {noreply, Reason};
    ok ->
      {noreply, Reason};
    init ->
      {noreply, Reason}
  end.


handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% Agent Supervisor API
%% concurrent request only.  Sets sync to false.
%% TODO: handler responses with receiver
concurrent_request(0, _) ->
  ok;
concurrent_request(NoConcurrentRequests, {Method,{Url,Headers,_ContentType,_Body},_,_}) when NoConcurrentRequests >= 1 ->
  RequestOptions = [{sync,false}],
  HttpOptions = [{timeout, 5000}, {connect_timeout, 5000}],
  gen_server:cast(?MODULE, {create_request,NoConcurrentRequests, {Method,{Url,Headers,_ContentType,_Body},HttpOptions,RequestOptions}}),
  concurrent_request(NoConcurrentRequests -1, {Method,{Url,Headers,_ContentType,_Body},HttpOptions,RequestOptions}).
