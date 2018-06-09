-module(oled_display).
-behaviour(gen_server).

-export([
          command/1,
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

command(Cmd) ->
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

handle_info(_Info, State) -> 
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
  cmd(P, ?DISPLAY_ON_CMD),
  timer:sleep(?DELAY),
  cmd(P, ?NORMAL_DISPLAY_CMD),
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
  cmd(P, integer_to_binary(Cmd));
cmd(P, Cmd) when is_binary(Cmd) ->
  i2c:write(P, <<?COMMAND_MODE, Cmd/binary>>).

data(P, Data) ->
  [i2c:write(P, <<?DATA_MODE, D/integer>>) || <<D:8>> <= Data].

pos(P, X, Y) ->
  cmd(P, 16#B0 + Y),
  cmd(P, 16#00 + (8 * X band 16#0F)),
  cmd(P, 16#10 + ((8 * X bsr 4) band 16#0F)).

brightness(P, Value) ->
  cmd(P, ?SET_BRIGHTNESS_CMD),
  cmd(P, Value).

mode(P, normal) ->
  cmd(P, ?NORMAL_DISPLAY_CMD);
mode(P, invert) ->
  cmd(P, ?INVERSE_DISPLAY_CMD);
mode(P, horizontal) ->
  cmd(P, 16#20),
  cmd(P, 16#00);
mode(P, page) ->
  cmd(P, 16#20),
  cmd(P, 16#02).  

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
  cmd(P, D),
  cmd(P, 16#00),
  cmd(P, StartPage),
  cmd(P, Sp),
  cmd(P, EndPage),
  cmd(P, 16#00),
  cmd(P, 16#FF),
  timer:sleep(?DELAY),
  cmd(P, ?ACTIVATE_SCROLL_CMD);
scroll(P, deactivate) ->
  cmd(P, ?DECTIVATE_SCROLL_CMD).  

string(P, Str) ->
  [char(P, I) || I <- Str].

char(P, Chr) when (Chr > 32) and (Chr < 127) ->
  data(P, lists:nth(Chr, ?FONT));
char(P, _) ->
  char(P, ' ').

clear(P) ->
  cmd(P, ?DISPLAY_OFF_CMD),
  lists:map(fun(J) ->
      pos(P, J, 0),
      string(P, lists:concat(lists:duplicate(16, " ")))
    end, lists:seq(0, 7, 1)),
  cmd(P, ?DISPLAY_ON_CMD),
  pos(P, 0, 0).
