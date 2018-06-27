-module(uart_gps).
-behaviour(gen_server).

-export([
          values/0,
          test/0,
          start_link/1, 
          init/1, 
          handle_call/3, 
          handle_cast/2, 
          handle_info/2, 
          terminate/2, 
          code_change/3
        ]).

-include("erlem.hrl").


values() ->
  gen_server:call(?MODULE, values).


test() -> 
  start_link(#{ bus => "ttyS0" }),
  timer:sleep(30000),
  values().


start_link(Params) -> 
  gen_server:start_link({local, ?MODULE}, ?MODULE, Params, []).

init(Params) -> 
  {ok, init_device(Params)}.

%
% gen_server
%

handle_call(values, _From, #{ current := Val } = State) ->  
  {reply, Val, State};
handle_call(_Msg, _From, State) ->  
  {reply, ok, State}.

handle_cast(_Msg, State) -> 
  {noreply, State}.

handle_info({uart, _Uart, Data}, State) ->
  case nmea0183:parse(Data) of
    undefined -> 
      {noreply, State};
    D ->
      {noreply, State#{ current := D }}
  end;
handle_info({uart_error, Uart, Info}, #{ params := Params }) ->
  ?LOG("UART Error - ~p", [Info]),
  uart:close(Uart),
  {noreply, init_device(Params)};
handle_info({uart_closed, _Uart}, #{ params := Params }) ->
  {noreply, init_device(Params)};
handle_info(_Info, State) -> 
  {noreply, State}.

terminate(_Reason, #{ port := undefined }) -> 
  ok;
terminate(_Reason, #{ port := Port }) -> 
  uart:close(Port),
  ok.

code_change(_OldVsn, State, _Extra) -> 
  {ok, State}.

%
% driver
%

init_device(#{ bus := Bus } = Params)  ->
  Port = case uart:open("/dev/" ++ Bus, [{baud, 9600}, {packet, line}, {active, true}]) of
    {ok, P} ->
      P;
    Error ->
      ?LOG("Can't start GPS agent - ~p", [Error]),
      undefined
  end,
  #{
    port => Port,
    params => Params,
    current => undefined
  }.

