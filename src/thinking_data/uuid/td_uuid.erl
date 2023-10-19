%%%-------------------------------------------------------------------
%%% @author ThinkingData
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 5Month 2022 10:19
%%%-------------------------------------------------------------------
-module(td_uuid).
-author("ThinkingData").

%% API
-export([v4/0, v4_string/0, to_string/1, get_parts/1, to_binary/1]).

%% generate UUID.
v4() ->
  v4(rand:uniform(round(math:pow(2, 48))) - 1, rand:uniform(round(math:pow(2, 12))) - 1, rand:uniform(round(math:pow(2, 32))) - 1, rand:uniform(round(math:pow(2, 30))) - 1).

%% generate UUID string.
-spec v4_string() -> string().
v4_string() ->
  to_string(v4()).

v4(R1, R2, R3, R4) ->
  <<R1:48, 4:4, R2:12, 2:2, R3:32, R4: 30>>.

%% convert UUID to string.
to_string(U) ->
  lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b", get_parts(U))).

%% return 32, 16, 16, 8, 8, 48 parts of a binary UUID.
get_parts(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>>) ->
  [TL, TM, THV, CSR, CSL, N].

%% convert UUID string to binary.
to_binary(U)->
  convert(lists:filter(fun(Elem) -> Elem /= $- end, U), []).

%% convert HEX to string.
convert([], Acc)->
  list_to_binary(lists:reverse(Acc));
convert([X, Y | Tail], Acc)->
  {ok, [Byte], _} = io_lib:fread("~16u", [X, Y]),
  convert(Tail, [Byte | Acc]).
