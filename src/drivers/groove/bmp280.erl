-module(bmp280).
-behaviour(gen_server).

-export([
          values/0,
          sealevel/2,
          altitude/1,
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
-include("drivers/bmp280.hrl").


values() ->
  gen_server:call(?MODULE, values).

sealevel(P, A) ->
  P / math:pow(1 - (A / 44330.0), 5.255).

altitude(P) ->
  altitude(P, 101325.0).

altitude(P, P0) ->
  44330.0 * (1 - math:pow(P / P0, 1 / 5.255)).


test() -> 
  start_link(#{ bus => "i2c-0", address => 16#77 }),
  values().


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
  #{
    proc => P,
    params => Params,
    calib => Calib
  }.


get_values(#{ proc := P, calib := Calib, params := Params }) ->
  case read(raw, maps:get(oversampling, Params, standart), P) of
    {0.0, 0.0} ->
      undefined;
    Values ->
      {TV, TFine} = calculate(t, Values, maps:get(t, Calib)),
      PV = calculate({p, TFine}, Values, maps:get(p, Calib)),
      #{ t => TV, p => PV }
  end.

calibration(P) ->
  #{
    t => calibration(?CAL_T, P),
    p => calibration(?CAL_P, P)
  }.

calibration(List, P) ->
  list_to_tuple(lists:map(fun({Type, Addr}) -> read(Type, Addr, P) end, List)).


calculate(t, {UT, _}, {T1, T2, T3}) ->
  Var1 = (((UT bsr 3) - (T1 bsl 1)) * T2) bsr 11,
  Var2 = (((((UT bsr 4) - T1) * ((UT bsr 4) - T1)) bsr 12) * T3) bsr 14,
  TFine = Var1 + Var2,
  T = ((TFine * 5 + 128) bsr 8) / 100.0,
  case (T < 100.0) and (T > - 100.0) of
    true -> {T, TFine};
    false -> {invalid, TFine}
  end;
calculate({p, TFine}, {_, UP}, {P1, P2, P3, P4, P5, P6, P7, P8, P9}) ->
  Var1 = TFine - 128000,
  Var2 = Var1 * Var1 * P6 + ((Var1 * P5) bsl 17) + (P4 bsl 35),
  Var3 = ((Var1 * Var1 * P3) bsr 8) + ((Var1 * P2) bsl 12),
  Var4 = (((1 bsl 47) + Var3) * P1) bsr 33,
  case Var4 of 
    0 ->
      invalid;
    _ ->
      P_1 = 1048576 - UP,
      P_2 = round((((P_1 bsl 31) - Var2) * 3125) / Var4),
      Var5 = (P9 * (P_2 bsr 13) * (P_2 bsr 13)) bsr 25,
      Var6 = (P8 * P_2) bsr 19,
      P = (((P_2 + Var5 + Var6) bsr 8) + (P7 bsl 4)) / 256,
      case (P < 120000.0) and (P > 80000.0) of
        true -> P;
        false -> invalid
      end
  end. 

read(raw, Os, P) ->
  {Mode, Delay} = case Os of
    ultralowpower -> {?MODE_ULTRALOWPOWER, 5};
    standart ->      {?MODE_STANDART, 8};
    hires ->         {?MODE_HIRES, 14};
    ultrahires ->    {?MODE_ULTRAHIRES, 26}
  end,
  Cmd = ?READ_CMD + (Mode bsl 6),
  ok = i2c:write(P, <<?REG_CONTROL, Cmd:8/integer>>),
  timer:sleep(Delay),
  <<D0:8, D1:8, D2:8, D3:8, D4:8, D5:8>> = read(6, ?REG_RESULT, P),
  UP = ((((D0 bsl 8) bor D1) bsl 8 ) bor D2) bsr 4,
  UT = ((((D3 bsl 8) bor D4) bsl 8 ) bor D5) bsr 4,
  {UT, UP};
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
  ok = i2c:write(P, <<Addr:8/integer>>),
  i2c:read(P, N).  
