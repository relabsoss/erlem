-module(bmp280).
-behaviour(gen_server).

-export([
          values/0,
          sealevel/2,
          altitude/2,
          start_link/1, 
          init/1, 
          handle_call/3, 
          handle_cast/2, 
          handle_info/2, 
          terminate/2, 
          code_change/3
        ]).

-include("erlem.hrl").
-include("drivers/bmp280.hrl").


values() ->
  gen_server:call(?MODULE, values).

sealevel(P, A) ->
  P / math:pow(1 - (A / 44330.0), 5.255).

altitude(P, P0) ->
  44330.0 * (1 - math:pow(P / P0, 1 / 5.255)).


start_link(Params) -> 
  gen_server:start_link({local, ?MODULE}, ?MODULE, Params, []).

init(Params) -> 
  {ok, init_device(Params)}.

%
% gen_server
%

handle_call(values, _From, State) ->  
  {reply, get_values(State), State};
handle_call(_Msg, _From, State) ->  
  {reply, ok, State}.

handle_cast(_Msg, State) -> 
  {noreply, State}.

handle_info(_Info, State) -> 
  {noreply, State}.

terminate(_Reason, _State) -> 
  ok.

code_change(_OldVsn, State, _Extra) -> 
  {ok, State}.

%
% driver
%

init_device(#{ bus := Bus } = Params)  ->
  {ok, P} = i2c:start_link(Bus, maps:get(address, Params, ?ADDRESS)),
  Calib = calibration(P),
  Delay = start_measurment(P, maps:get(oversampling, Params, 0)),
  #{
    proc => P,
    params => Params,
    calib => Calib,
    delay => Delay
  }.

start_measurment(P, Os) ->
  {Cmd, Delay} = case Os of 
    1 -> {?COMMAND_PRESSURE1, 10};
    2 -> {?COMMAND_PRESSURE2, 15};
    3 -> {?COMMAND_PRESSURE3, 24};
    4 -> {?COMMAND_PRESSURE4, 45};
    _ -> {?COMMAND_PRESSURE0, 8}
  end,
  ok = i2c:write(P, <<?REG_CONTROL, Cmd:8/integer>>),
  Delay.


get_values(#{ proc := P, calib := Calib }) ->
  case read(uncal, P) of
    {0.0, 0.0} ->
      undefined;
    Values ->
      {T, TFine} = calculate(t, Values, maps:get(t, Calib)),
      P = calculate({p, TFine}, Values, maps:get(p, Calib)),
      #{ t => T, p => P }
  end.

calibration(P) ->
  #{
    t => calibration(?CAL_T, P),
    p => calibration(?CAL_P, P)
  }.

calibration(List, P) ->
  list_to_tuple(lists:map(fun({Type, Addr}) -> read(Type, Addr, P) end, List)).


calculate(t, {UT, _}, {T1, T2, T3}) ->
  Var1 = UT / 16384.0 - T1 / 1024.0 * T2,
  Var2 = UT / 131072.0 - T1 / 8192.0 * UT / 131072.0 - T1 / 8192.0 * T3,
  T = (Var1 + Var2) / 5120.0,
  TFine = Var1 + Var2,
  case (T < 100.0) and (T > - 100.0) of
    true -> {T, TFine};
    false -> {invalid, TFine}
  end;
calculate({p, TFine}, {_, UP}, {P1, P2, P3, P4, P5, P6, P7, P8, P9}) ->
  Var1 = TFine / 2.0 - 64000.0,
  Var2 = Var1 * Var1 * P6 / 32768.0,
  Var3 = Var2 + Var1 * P5 * 2.0, 
  Var4 = Var3 / 4.0 + P4 * 65536.0,
  Var5 = P3 * Var1 * Var1 / 524288.0 + P2 * Var1 / 524288.0,
  %TVar = (32768.0 + Var5) / 32768.0,
  %TTVar = TVar * P1,
  Var6 = (32768.0 + Var5) / 32768.0 * P1,
  P_1 = 1048576.0 - UP,
  P_2 = (P_1 - (Var4 / 4096.0)) * 6250.0 / Var6,
  Var7 = P9 * P_2 * P_2 / 2147483648.0,
  Var8 = P_2 * P8 / 32768.0,
  P = (P_2 + (Var7 + Var8 + P7) / 16.0) / 100.0,
  case (P < 1200.0) and (P > 800.0) of
    true -> P;
    false -> invalid
  end. 

read(uncal, P) ->
  <<D0:8, D1:8, D2:8, D3:8, D4:8, D5:8>> = read(6, ?REG_RESULT_PRESSURE, P),
  Factor = math:pow(2, 4),
  UP = ((D0 * 256.0) + D1 + (D2 / 256.0)) * Factor, 
  UT = ((D3 * 256.0) + D4 + (D5 / 256.0)) * Factor,
  {UT, UP}.

read(byte, Addr, P) ->
  <<B:1/little-unsigned-integer-unit:8>> = read(1, Addr, P),
  B;
read(int, Addr, P) ->
  <<B:1/little-signed-integer-unit:16>> = read(2, Addr, P),
  B;
read(uint, Addr, P) ->
  <<B:1/little-unsigned-integer-unit:16>> = read(2, Addr, P),
  B;  
read(N, Addr, P) when is_integer(N) ->
  ok = i2c:write(P, Addr),
  i2c:read(P, N).  
