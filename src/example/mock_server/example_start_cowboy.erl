%%%-------------------------------------------------------------------
%%% @author ThinkingData
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%% Example
%%% @end
%%% Created : 22. 3月 2023 21:21
%%%-------------------------------------------------------------------
%%%
%%% need import {cowboy, {git, "https://github.com/ninenines/cowboy.git", {tag, "2.9.0"}}}

-module(example_start_cowboy).
-author("ThinkingData").

%% API
-export([start/0]).

start() ->
  CowboyDispatch = cowboy_router:compile([
    {'_', [
      {"/track", te_cowboy_handler, ["arg_1", "arg_2"]}
    ]}
  ]),
  cowboy:start_clear(http, [{port, 18082}], #{
    env => #{dispatch => CowboyDispatch},
    idle_timeout => 500000
  }).