-module(erlem_sup).
-behaviour(supervisor).
-export([
          start_link/0,
          init/1
        ]).

-include("erlem.hrl").


start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, {{one_for_one, 500, 10000}, lists:flatten([apply(M) || M <- ?CONFIG(modules, [])])}}.

apply({Module, ConfigName}) ->
  ?CHILD(Module, worker, [ConfigName]);
apply(Module) ->
  ?CHILD(Module, worker).
