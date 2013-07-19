%% @author dadean
%% @doc This module generates requests.  In charge of creating generator slaves which actually send the requests out.  Each initiator is responsible for one request chain. Input: list of tuples in the following format: {Module, Function, Arguments}.  An example of a tuple is {generator_slave,start,[RequestTuple1,RequestTuple2,RequestTuple3..,RequestTupleN]}.
%% @version 0.0.1
-module(generator_request_initiator).


-export([start_link/2, stop/1]).
-export([init/1]).

% start_link takes in a name of supervisor module and a list of request slaves
start_link(Name, ChildSpecList) -> 
  register(Name, spawn_link(
    generator_request_initiator, init, [ChildSpecList])), 
  ok. 

% start trapping exits
% start all slaves
init(ChildSpecList) -> 
  process_flag(trap_exit, true), 
  loop(start_children(ChildSpecList)). 

% start all slaves internal function
start_children([]) -> []; 
start_children([{M, F, A} | ChildSpecList]) -> 
  case (catch apply(M, F, A)) of 
    {ok, Pid} -> 
      [{Pid, {M, F, A}} | start_children(ChildSpecList)]; 
    _ -> 
      start_children(ChildSpecList) 
  end.

restart_child(Pid, ChildList) -> 
  {value, {Pid, {M, F, A}}} = lists:keysearch(Pid, 1, ChildList), 
  {ok, NewPid} = apply(M, F, A), 
  [{NewPid, {M, F, A}} | lists:keydelete(Pid, 1, ChildList)]. 

% wait for response from child list
% if response is 'EXIT' then restart the child
% if response is stop then terminate the supervisor
% if response is complete then terminate ONLY that slave
loop(ChildList) -> 
  receive 
    {' EXIT', Pid, _Reason} -> 
      NewChildList = restart_child(Pid, ChildList), 
      loop(NewChildList); 
    {complete, Pid, _Reason} -> % stop slave and remove it from list
      lists:keydelete(Pid, 1, ChildList),
      exit(Pid, finished_processing);
    {execute_in_parallel, Request} ->
      ok;     
    {stop, From} -> 
      From ! {reply, terminate(ChildList)} 
  end.

% make Number of requests in parallel, using child lists
execute_in_parallel(Number, Request) when Number > 0 ->  
  {value, {Pid, {M, F, A}}} = lists:keysearch(Pid, 1, ChildList), 
  ok.

% make RequestList in series with pattern matching in between
execute_in_series(RequestList) -> ok.

% stops the supervisor
stop(Name) -> 
  Name ! {stop, self()}, 
  receive 
    {reply, Reply} -> Reply 
  end. 

terminate([{Pid, _} | ChildList]) -> 
  exit(Pid, kill), 
  terminate(ChildList); 
terminate(_ChildList) -> ok.


