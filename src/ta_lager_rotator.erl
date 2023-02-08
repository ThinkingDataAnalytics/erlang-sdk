%%%-------------------------------------------------------------------
%%% @author yxiong
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 10M 2022 14:37
%%%-------------------------------------------------------------------
-module(ta_lager_rotator).
-author("yxiong").

-include_lib("kernel/include/file.hrl").

-behaviour(lager_rotator_behaviour).

-export([
  create_logfile/2, open_logfile/2, ensure_logfile/5, rotate_logfile/2
]).

create_logfile(Name, Buffer) ->
  {{Y, M, D}, {H, _, _}} = calendar:now_to_local_time(os:timestamp()),
  DateHour =  {Y, M, D, H},
  FileName = getFilename(Name, DateHour, 0),
  filelib:ensure_dir(Name),
  file:delete(Name),
  file:make_symlink(filename:absname(FileName), Name),
  open_logfile(Name, Buffer).

rotate_logfile(Name, _Count) ->
  case file:read_link(Name) of
    {ok, LinkedName} ->
      case filelib:file_size(LinkedName) of
        0 ->
          %% if the files size is zero, it is removed
          catch file:delete(LinkedName);
        _ ->
          void
      end;
    _ ->
      void
  end,
  {ok, {FD, _, _, _}} = create_logfile(Name, []),
  file:close(FD).

open_logfile(Name, Buffer) ->
  case filelib:ensure_dir(Name) of
    ok ->
      Options = [append, raw] ++
        case Buffer of
          {Size0, Interval} when is_integer(Interval), Interval >= 0, is_integer(Size0), Size0 >= 0 ->
            [{delayed_write, Size0, Interval}];
          _ -> []
        end,
      case file:open(Name, Options) of
        {ok, FD} ->
          case file:read_file_info(Name, [raw]) of
            {ok, FInfo0} ->
              Inode = FInfo0#file_info.inode,
              {ok, Ctime} = maybe_update_ctime(Name, FInfo0),
              Size1 = FInfo0#file_info.size,
              {ok, {FD, Inode, Ctime, Size1}};
            X -> X
          end;
        Y -> Y
      end;
    Z -> Z
  end.

reopen_logfile(Name, FD0, Buffer) ->
  %% Flush and close any file handles.
  %% delayed write can cause file:close not to do a close
  _ = file:datasync(FD0),
  _ = file:close(FD0),
  _ = file:close(FD0),
  case open_logfile(Name, Buffer) of
    {ok, {_FD1, _Inode, _Size, _Ctime}=FileInfo} ->
      %% inode changed, file was probably moved and
      %% recreated
      {ok, FileInfo};
    Error ->
      Error
  end.

ensure_logfile(Name, FD, Inode, Ctime, Buffer) ->
  case file:read_link(Name) of
    {ok, _} ->
      lager_ensure_logfile(Name, FD, Inode, Ctime, Buffer);
    _ ->
      create_logfile(Name, Buffer)
  end.

lager_ensure_logfile(Name, undefined, _Inode, _Ctime, Buffer) ->
  open_logfile(Name, Buffer);
lager_ensure_logfile(Name, FD, Inode, Ctime, Buffer) ->
  case lager_util:has_file_changed(Name, Inode, Ctime) of
    {true, _FInfo} ->
      reopen_logfile(Name, FD, Buffer);
    {_, FInfo} ->
      {ok, {FD, Inode, Ctime, FInfo#file_info.size}}
  end.

%% @doc Create name of a new file
%% @private
getFilename(BaseFileName, DateHour, Branch) ->
  FileName = lists:append([BaseFileName, ".", suffix(DateHour, true), "_", integer_to_list(Branch)]),
  case filelib:is_file(FileName) of
    true ->
      getFilename(BaseFileName, DateHour, Branch + 1);
    _ ->
      FileName
  end.

%% @doc Zero-padding number
%% @private
zeropad(Num, MinLength) ->
  NumStr = integer_to_list(Num),
  zeropad_str(NumStr, MinLength - length(NumStr)).
zeropad_str(NumStr, Zeros) when Zeros > 0 ->
  zeropad_str([$0 | NumStr], Zeros - 1);
zeropad_str(NumStr, _) ->
  NumStr.

%% @doc Create a suffix
%% WithHour: true/false, add Hour to filename or not
%% @private
suffix({Y, M, D, H}, WithHour) ->
  YS = zeropad(Y, 4),
  MS = zeropad(M, 2),
  DS = zeropad(D, 2),
  HS = zeropad(H, 2),
  case WithHour of
    true ->
      lists:flatten([YS, "-", MS, "-", DS, "-", HS]);
    _ ->
      lists:flatten([YS, "-", MS, "-", DS])
  end.

maybe_update_ctime(Name, FInfo) ->
  {OsType, _} = os:type(),
  do_update_ctime(OsType, Name, FInfo).

do_update_ctime(win32, Name, FInfo0) ->
  % Note: we force the creation time to be the current time.
  % On win32 this may prevent the ctime from being updated:
  % https://stackoverflow.com/q/8804342/1466825
  NewCtime = calendar:local_time(),
  FInfo1 = FInfo0#file_info{ctime = NewCtime},
  ok = file:write_file_info(Name, FInfo1, [raw]),
  {ok, NewCtime};
do_update_ctime(_, _Name, FInfo) ->
  {ok, FInfo#file_info.ctime}.
