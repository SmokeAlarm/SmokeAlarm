%%%-----------------------------------------------------------------------------
%%%
%%% Desc:       Predefined SmokeAlarm error_logger reactor
%%%             
%%% Purpose:    Provide a bidge between SmokeAlarm and SASL alarms by...
%%%             - Setting an alarm in current SASL alarm_handler when a
%%%               SmokeAlarm detector trips.
%%%             - Clearing it when the detector resets.
%%%
%%%             TODO: Provide a more useful replacement for the default SASL
%%%             alarm_handler (possibly periodic multi-file persistence
%%%             using log_mf_h)
%%%
%%%-----------------------------------------------------------------------------

-module(sa_stdreactor_errlog).
-legal("Copyright (c) 2011, The Authors.\n"
        "http://github.com/SmokeAlarm\n"
        "See LICENSE.txt for licensing information.").
-author("Edmond Begumisa <ebegumisa at hysteria dash tech dot com>").

%% Public Interface
-export([handle_tripped/3, handle_reset/3]).

%%% Interface ------------------------------------------------------------------

handle_tripped(ReactorId, TrippedResult, []) ->
    handle_tripped(ReactorId, TrippedResult, [info_msg]);
handle_tripped(ReactorId, TrippedResult, [LogType]) ->
    log(ReactorId, tripped, TrippedResult, LogType).

handle_reset(ReactorId, ResetResult, []) ->
    handle_reset(ReactorId, ResetResult, [info_msg]);
handle_reset(ReactorId, ResetResult, [LogType]) ->
    log(ReactorId, reset, ResetResult, LogType).

log({DetectorKey,DetectorPid,ReactorKey}, State, Result, LogType) when
    DetectorPid=:=self() % Emphasise that we mustn't retrigger sa_stdfeed_errlog if being used
    ->
    IsMsg = case LogType of
                info_msg -> true;
                error_msg -> true;
                warning_msg -> true;
                info_report -> false;
                error_report -> false;
                warning_report -> false;
                _ -> throw({badarg,LogType})
            end,
    if
        IsMsg ->
            error_logger:LogType("SmokeAlarm Reactor ~p: "
                                 "Detector ~p has ~w.~n"
                                 "Result: ~p~n",
                                 [ReactorKey, DetectorKey, State, Result]);
        true ->
            error_logger:LogType([{smokealarm, 'reactor'},
                                  {DetectorKey, State},
                                  {'result', Result},
                                  {'reactor_key', ReactorKey}])
    end.