%%%-----------------------------------------------------------------------------
%%%
%%% Desc:       Predefined SmokeAlarm error_logger reactor
%%%             
%%% Purpose:    Provide a bidge between SmokeAlarm and SASL alarms by...
%%%             - Setting an alarm in current SASL alarm_handler when a
%%%               SmokeAlarm detector trips (starting sasl if it's not running).
%%%             - Clearing it when the detector resets.
%%%
%%%             TODO: Install a more useful replacement for the default SASL
%%%             alarm_handler (possibly periodic multi-file persistence
%%%             using log_mf_h)
%%%
%%%-----------------------------------------------------------------------------

-module(sa_stdreactor_saslah).
-legal("Copyright (c) 2011, The Authors.\n"
        "http://github.com/SmokeAlarm\n"
        "See LICENSE.txt for licensing information.").
-author("Edmond Begumisa <ebegumisa at hysteria dash tech dot com>").

%% Public Interface
-export([handle_tripped/4, handle_reset/3]).
                
%%% Interface ------------------------------------------------------------------

handle_tripped({DetectorKey,_,_}=_ReactorId, TrippedResult, [], _PipeAcc) ->
    try
        alarm_handler:set_alarm({DetectorKey, TrippedResult})
    catch error:badarg ->
        case application:start(sasl) of
            ok -> ok;
            {error, {already_started,sasl}} -> ok
        end,
        alarm_handler:set_alarm({DetectorKey, TrippedResult})
    end.
    
handle_reset({DetectorKey,_,_}=_ReactorId, _ResetResult, []) ->
    alarm_handler:clear_alarm(DetectorKey).

 