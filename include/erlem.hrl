%
% Common project options
%

-define(AFTER(Timeout, Event), {ok, _} = timer:send_after(Timeout, Event)).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 50, Type, [I]}).
-define(CHILD(I, Type, Param), {I, {I, start_link, Param}, permanent, 50, Type, [I]}).
-define(CHILD(Id, I, Type, Param), {Id, {I, start_link, Param}, permanent, 50, Type, [I]}).

-define(CONFIG(Key, Default), application:get_env(erlem, Key, Default)).
%
% Logger
%

-define(LOG(Msg, Pars), error_logger:warning_msg(Msg, Pars)).
-define(DEBUG(Msg, Pars), io:format(Msg ++ "~n", Pars)).
