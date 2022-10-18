%%%-------------------------------------------------------------------
%% @doc thinkingdata_analytics public API
%% @end
%%%-------------------------------------------------------------------

-module(thinkingdata_analytics_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    thinkingdata_analytics_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
