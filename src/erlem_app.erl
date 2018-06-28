-module(erlem_app).
-behaviour(application).

-export([
          start/2, 
          stop/1,
          priv_dir/0
        ]).

-include("erlem.hrl").


start(_StartType, _StartArgs) ->
  Priv = priv_dir(),
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/[...]", cowboy_static, {priv_dir, erlem, "www"}},
      {"/", cowboy_static, {file, filename:join([Priv, "www", "index.html"])}}
    ]}
  ]),
  {ok, _} = cowboy:start_clear(erlem_http_listener,
        [{port, ?CONFIG(listen_port, 8080)}],
        #{ env => #{ dispatch => Dispatch } }
    ),
  erlem_sup:start_link().

stop(_State) ->
  ok.

root_dir() ->
  Ebin = filename:dirname(code:which(?MODULE)),
  filename:dirname(Ebin).

priv_dir() ->
  filename:join(root_dir(), "priv").
