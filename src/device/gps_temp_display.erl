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
    #{ lat := Lt, lng := Ln, time := undefined } ->
      {Lt, Ln, [0, 0, 0]};
    #{ lat := Lt, lng := Ln, time := Tm } ->
      {Lt, Ln, Tm};
    _ ->
      {0.0, 0.0, [0, 0, 0]}
  end,
  oled_display:p({text, {0, 1, format("Temp ~*.*.*f", [11, 2, $  , T])}}),
  oled_display:p({text, {0, 2, format("Pres ~*.*.*f", [11, 2, $  , P])}}),
  oled_display:p({text, {0, 3, format("Lat  ~*.*.*f", [11, 5, $  , Lat])}}),
  oled_display:p({text, {0, 4, format("Lng  ~*.*.*f", [11, 5, $  , Lng])}}),
  oled_display:p({text, {0, 5, format("Time    ~2.10.0B:~2.10.0B:~2.10.0B", Time)}}).

format(F, V) ->
  lists:flatten(io_lib:format(F, V)).
