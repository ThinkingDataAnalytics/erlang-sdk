%%%-------------------------------------------------------------------
%%% @author ThinkingData
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%% Example
%%% @end
%%% Created : 22. 3月 2023 21:21
%%%-------------------------------------------------------------------
-module(example_cowboy_handler).
-author("ThinkingData").

-import(lists,[nth/2]).

%% API
-export([init/2]).

init(Req, Opts) ->
  Arg1 = nth(1, Opts),
  Arg2 = nth(2, Opts),

  {ok, Data, _} = cowboy_req:read_body(Req),
  Path = cowboy_req:path(Req),
  case Path of
    <<"/track">> -> track(Req, Data, Arg1, Arg2)
  end,
  {ok, Req, Opts}.

track(Req, Data, Arg1, Arg2) ->
  io:format("data:~n~w~n", [Data]),
  io:format("Arg1:~n~w~n", [Arg1]),
  io:format("Arg2:~n~w~n", [Arg2]),

  cowboy_req:reply(200, #{
    <<"content-type">> => <<"text/plain; charset=utf-8">>
  }, <<"track">>, Req).
