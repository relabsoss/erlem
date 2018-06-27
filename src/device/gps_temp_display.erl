-module(gps_temp_display).
-behaviour(gen_server).

-export([
          start_link/1, 
          init/1, 
          handle_call/3, 
          handle_cast/2, 
          handle_info/2, 
          terminate/2, 
          code_change/3
        ]).

-include("erlem.hrl").


start_link(Params) -> 
  gen_server:start_link({local, ?MODULE}, ?MODULE, Params, []).

init(Params) -> 
  ?AFTER(maps:get(timestep, Params, 1000), show),
  {ok, Params}.

%
% gen_server
%

handle_call(_Msg, _From, State) ->  
  {reply, ok, State}.

handle_cast(_Msg, State) -> 
  {noreply, State}.

handle_info(show, State) ->
  show(),
  ?AFTER(maps:get(timestep, State, 1000), show),
  {noreply, State};
handle_info(_Info, State) -> 
  {noreply, State}.

terminate(_Reason, _State) -> 
  ok.

code_change(_OldVsn, State, _Extra) -> 
  {ok, State}.

%
% local
%

show() ->
  #{ t := T, p := P } = bmp280:values(),
  {Lat, Lng, Time} = case uart_gps:values() of
    #{ lat := Lt, lng := Ln, time := Tm } ->
      {Lt, Ln, Tm};
    undefined ->
      {0.0, 0.0, [0, 0, 0]}
  end,
  oled_display:p({text, {0, 0, format("Temp ~*.*.*f", [9, 2, ' ', T])}}),
  oled_display:p({text, {0, 1, format("Pres ~*.*.*f", [9, 2, ' ', P])}}),
  oled_display:p({text, {0, 2, format("Lat  ~*.*.*f", [9, 5, ' ', Lat])}}),
  oled_display:p({text, {0, 3, format("Lng  ~*.*.*f", [9, 5, ' ', Lng])}}),
  oled_display:p({text, {0, 4, format("Time ~2.10B:~2.10B:~2.10B", Time)}}).

format(F, V) ->
  lists:concat(io_lib:format(F, V)).
