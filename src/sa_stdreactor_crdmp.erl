%%%-----------------------------------------------------------------------------
%%%
%%% Desc:       Predefined SmokeAlarm crashdump reactor.
%%%             
%%% Purpose:    - Avail a way for obtaining an in-memory (partial) crash dump
%%%               readable by the OTP CrashDump Viewer when a detector trips
%%%               (i.e. without the emulator actually crashing)
%%%             - TODO: Optionally save it to disk.
%%%             - Pipe it to the next reactor.
%%%             Do nothing if a detector resets.
%%%
%%%-----------------------------------------------------------------------------

-module(sa_stdreactor_crdmp).
-legal("Copyright (c) 2011, The Authors.\n"
        "http://github.com/SmokeAlarm\n"
        "See LICENSE.txt for licensing information.").
-author("Edmond Begumisa <ebegumisa at hysteria dash tech dot com>").

%% Public Interface
-export([handle_tripped/4, handle_reset/3]).

-export([dump/1]).

%% Private Preprocess
-include("sa_debug.hrl").

%%% Interface ------------------------------------------------------------------

handle_tripped(ReactorId, _TrippedResult, [], PipeAcc) ->
    {pipe, [dump(ReactorId) | PipeAcc]}.
    
handle_reset(_ReactorId, _ResetResult, []) ->
    ok.

dump(ReactorId) ->
    MiscB = erlang:system_info(info),
    Header = io_lib:format(
        "=erl_crash_dump:0.1~n"
        "~s~n"                  % Dump time
        "Slogan: SmokeAlarm: User generated dump created by reactor ~w.~n"
        "System version: ~s"    
        "Compiled: ~n"          % ?? There doesn't seem to be a way of getting the emulator compile time from Erlang
        "Taints: ~n"            % ?? erl_crash_dump_v() in break.c currently leaves it empty!
        "Atoms: ~s~n",
        [sa_debug:format_time(erlang:localtime()),
         ReactorId,
         erlang:system_info(system_version),
         extract_atom_cnt(MiscB)]),
    <<(iolist_to_binary(Header))/binary,
      MiscB/binary,
      (dump_ets())/binary,
      % ?? erts_print_bif_timer_info(fd, NULL);
      (erlang:system_info(procs))/binary,
      (erlang:system_info(dist))/binary,
      (dump_loaded())/binary>>.

extract_atom_cnt(MiscB) ->
    Matches = [<<"=hash_table:atom_tab">>, <<"entries: ">>, <<"\n">>],
    case sa_utils:binary_ordered_match(MiscB, Matches) of
        [{_,_},{Pos2,Len2},{Pos3,1}] ->
            binary:part(MiscB, {Pos2+Len2, Pos3-(Pos2+Len2)});
        _ ->
            <<>>
    end.

dump_ets() ->
    Tabs = lists:reverse(ets:all()),
    TabTriples = [{Slot, Tab, ets:info(Tab)} || {Slot, Tab} <-
                    lists:zip(lists:seq(1, length(Tabs)), Tabs)],
    iolist_to_binary([dump_ets(X) || X <- TabTriples]).
    
dump_ets({Slot, Tab, TabInfo}) ->
    TabFmt = "=ets:~w~n"
             "Slot: ~w~n"
             "Table: ~w~n"
             "Name: ~w~n"
             "~s~n"
             "Objects: ~w~n"
             "Words: ~w~n",
    {owner, Owner} = lists:keyfind(owner, 1, TabInfo),
    {name, Name} = lists:keyfind(name, 1, TabInfo),
    {size, Objects} = lists:keyfind(size, 1, TabInfo),
    {memory, Words} = lists:keyfind(memory, 1, TabInfo),
    try io_lib:format(TabFmt,
        [Owner, Slot, Tab, Name,
            case lists:keyfind(type, 1, TabInfo) of
                {type, ordered_set} ->
                    io_lib:format("Ordered set (AVL tree), Elements: ~w",
                        [Objects]);
                _ ->
                    "Buckets: " % ??
            end,
            Objects, Words])
    catch error:_ -> TabFmt  % Not concat'ing "BAD FORMAT" so dump remains readable by CrashDumpViewer
    end.
    
dump_loaded() ->
    LoadedB1 = re:replace(erlang:system_info(loaded),
                            "(^\\w+)( [0-9]+)",
                            "=mod:\\1\nCurrent size:\\2",
                          [global, multiline]),
    LoadedB2 = re:replace(LoadedB1, "\\(([0-9]+) old\\)", "\nOld size: \\1",
                          [global]),
    iolist_to_binary([<<"=loaded_modules\n">>, LoadedB2]).
    