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
  VRoutes = [
    {"/[...]", cowboy_static, {dir, filename:join([Priv, "www"])}},
    {"/", cowboy_static, {dir, filename:join([Priv, "www", "index.html"])}}
  ],
  Dispatch = cowboy_router:compile([{'_',  VRoutes}]),
  cowboy:start_http(webapp_http_listener, 1,
    [{port, ?CONFIG(listen_port, 8080)}],
    [
      {env, [{dispatch, Dispatch}]}
    ]),
  erlem_sup:start_link().

stop(_State) ->
  ok.

root_dir() ->
  Ebin = filename:dirname(code:which(?MODULE)),
  filename:dirname(Ebin).

priv_dir() ->
  filename:join(root_dir(), "priv").
