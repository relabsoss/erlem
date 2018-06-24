-module(oled_display).
-behaviour(gen_server).

-export([
          p/1,
          start_link/1, 
          init/1, 
          handle_call/3, 
          handle_cast/2, 
          handle_info/2, 
          terminate/2, 
          code_change/3
        ]).

-include("erlem.hrl").
-include("drivers/oled_display.hrl").

p(Cmd) ->
  gen_server:cast(?MODULE, Cmd).

start_link(Params) -> 
  gen_server:start_link({local, ?MODULE}, ?MODULE, Params, []).

init(Params) -> 
  {ok, init_device(Params)}.

%
% gen_server
%

handle_call(_Msg, _From, State) ->  
  {reply, ok, State}.

handle_cast({Cmd, Params}, State) -> 
  {noreply, process(Cmd, Params, State)};
handle_cast(_Msg, State) -> 
  {noreply, State}.

handle_info(Info, State) -> 
  {noreply, State}.

terminate(_Reason, _State) -> 
  ok.

code_change(_OldVsn, State, _Extra) -> 
  {ok, State}.

%
% driver
%

init_device(#{ bus := Bus } = Params) ->
  {ok, P} = i2c:start_link(Bus, ?ADDRESS),
  cmd(P, ?DISPLAY_OFF_CMD),
  timer:sleep(?DELAY),
  clear(P),
  cmd(P, ?DISPLAY_ON_CMD),
  timer:sleep(?DELAY),
  mode(P, horizontal),
  mode(P, normal),
  #{
    proc => P,
    mode => none,
    params => Params
  }.

process(clear, _, #{ proc := P } = State) ->
  clear(P),
  State;
process(mode, Type, #{ proc := P } = State) ->
  mode(P, Type),
  State;
process(pos, {X, Y}, #{ proc := P } = State) ->
  pos(P, X, Y),
  State;
process(brightness, Value, #{ proc := P } = State) ->
  brightness(P, Value),
  State;
process(text, {X, Y, String}, #{ proc := P } = State) ->
  pos(P, X, Y),
  string(P, String),
  State;
process(text, String, #{ proc := P } = State) when is_list(String) ->
  string(P, String),
  State;
process(scroll, Params, #{ proc := P } = State) ->
  scroll(P, Params),
  State;
process(_, _, State) ->
  State.


cmd(P, Cmd) when is_integer(Cmd) ->
  cmd(P, <<Cmd/integer>>);
cmd(P, Cmd) when is_binary(Cmd) ->
  ok = i2c:write(P, <<?COMMAND_MODE, Cmd/binary>>).

data(_P, <<>>) ->
  ok;
data(P, Data) ->
  ok = i2c:write(P, <<?DATA_MODE, Data/binary>>).

pos(P, X, Y) ->
  cmd(P, 16#B0 + Y),
  cmd(P, 16#00 + (8 * X band 16#0F)),
  cmd(P, 16#10 + ((8 * X bsr 4) band 16#0F)).

brightness(P, Value) ->
  cmd(P, <<?SET_BRIGHTNESS_CMD, Value/integer>>).

mode(P, normal) ->
  cmd(P, ?NORMAL_DISPLAY_CMD);
mode(P, invert) ->
  cmd(P, ?INVERSE_DISPLAY_CMD);
mode(P, horizontal) ->
  cmd(P, <<16#20, 16#00>>);
mode(P, page) ->
  cmd(P, <<16#20, 16#02>>).  

scroll(P, {activate, Direction, StartPage, EndPage, Speed}) ->
  D = case Direction of
    left -> ?SCROLL_LEFT_CMD;
    right -> ?SCROLL_RIGHT_CMD
  end,
  Sp = case Speed of
    3 -> ?SCROLL_3FRAMES;
    4 -> ?SCROLL_4FRAMES;
    5 -> ?SCROLL_5FRAMES;
    25 -> ?SCROLL_25FRAMES;
    64 -> ?SCROLL_64FRAMES;
    128 -> ?SCROLL_128FRAMES;
    256 -> ?SCROLL_256FRAMES;
    _ -> ?SCROLL_2FRAMES
  end,
  cmd(P, <<D/integer, 16#00, StartPage/integer, Sp/integer, EndPage/integer, 16#00, 16#FF>>),
  timer:sleep(?DELAY),
  cmd(P, ?ACTIVATE_SCROLL_CMD);
scroll(P, deactivate) ->
  cmd(P, ?DECTIVATE_SCROLL_CMD).  

string(P, Str) ->
  [char(P, I) || I <- Str].

char(P, Chr) ->
  Pos = case (Chr > 31) and (Chr < 128) of
    true -> Chr;
    false -> ' '
  end,
  data(P, lists:nth(Pos - 31, ?FONT)).

clear(P) ->
  cmd(P, ?DISPLAY_OFF_CMD),
  lists:map(fun(J) ->
      pos(P, 0, J),
      string(P, lists:concat(lists:duplicate(16, ' ')))
    end, lists:seq(0, 7, 1)),
  cmd(P, ?DISPLAY_ON_CMD),
  pos(P, 0, 0).
