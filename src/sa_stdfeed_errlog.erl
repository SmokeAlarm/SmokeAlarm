%%%-----------------------------------------------------------------------------
%%%
%%% Desc:       Predefined SmokeAlarm error_logger feed 
%%%             
%%% Purpose:    Relay info/warning/error/emulator events (that do NOT originate
%%%             from SmokeAlarm itself) from error_logger to one or more
%%%             SmokeAlarm detectors.
%%%
%%%-----------------------------------------------------------------------------

-module(sa_stdfeed_errlog).
-legal("Copyright (c) 2011, The Authors.\n"
        "http://github.com/SmokeAlarm\n"
        "See LICENSE.txt for licensing information.").
-author("Edmond Begumisa <ebegumisa at hysteria dash tech dot com>").

%% Public Interface
-export([subscribe/2, unsubscribe/2]).

%% Private Interface
-behaviour(gen_event).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2,
         code_change/3]).

%% Private Preprocess
-record(relay, {info_msg, info_report, warning_msg, warning_report, error,
                error_report, emulator}).
-record(state, {subr_id, top_sa_sup_pid, relay}).
-include("common.hrl").
                
%%% Interface ------------------------------------------------------------------

subscribe(SubscriberId, [{relay, RelayL}]) ->
    do_subscribe(SubscriberId, to_relay_rec(RelayL));
subscribe(SubscriberId, []) ->
    do_subscribe(SubscriberId, to_relay_rec([])).
    
do_subscribe({_,DetP,_}=SubscriberId, #relay{}=Relay) when is_pid(DetP) ->
    S = #state{subr_id = SubscriberId, top_sa_sup_pid = whereis(sa_sup),
               relay = Relay},
    {ok = gen_event:add_handler(error_logger, {?MODULE,SubscriberId}, S), null}.

unsubscribe(SubscriberId, null) ->
    case gen_event:delete_handler(error_logger, {?MODULE,SubscriberId}, []) of
        #state{} -> ok;
        Error -> Error
    end.

%%% 'gen_event' callbacks ------------------------------------------------------

init(#state{}=S) ->
    {ok, S}.

% Ignore everything from owner detector
handle_event({_,_,{DetP,_,_}}, #state{subr_id={_,DetP,_}}=S) ->
    {ok, S}; 
handle_event({DetP,_,_}, #state{subr_id={_,DetP,_}}=S) ->
    {ok, S}; 
% Ignore everything from top SmokeAlarm sup
handle_event({_,_,{TopSASupP,_,_}}, #state{top_sa_sup_pid=TopSASupP}=S) when
             is_pid(TopSASupP)
    ->
    {ok, S};
handle_event({TopSASupP,_,_}, #state{top_sa_sup_pid=TopSASupP}=S) when
             is_pid(TopSASupP)
    ->
    {ok, S};
% Relay depending on content
handle_event({emulator,_,_}, #state{relay=#relay{emulator=false}}=S) ->
    {ok, S};
handle_event({emulator,_,_}=E, #state{subr_id=SubscriberId}=S) ->
    {ok = smokealarm:report_event(SubscriberId, E), S};
handle_event({info_msg,_,_}, #state{relay=#relay{info_msg=false}}=S) ->
    {ok, S};
handle_event({info_msg,_,_}=E, #state{subr_id=SubscriberId}=S) ->
    {ok = smokealarm:report_event(SubscriberId, E), S};
handle_event({info_report,_,_}, #state{relay=#relay{info_report=false}}=S) ->
    {ok, S};
handle_event({info_report,_,About}=E, #state{}=S) ->
    case About of
        {_,std_info,[{application,?APP}|_]} -> ignore; % App controller reporting on SA
        {_,progress,[{application,?APP}|_]} -> ignore; %  " "
        {_,std_info,[{?APP,_}|_]} -> ignore;  % sa_debug.hrl ?INFO macros
        _ -> ok = smokealarm:report_event(S#state.subr_id, E)
    end,
    {ok, S};
handle_event({warning_msg,_,_}, #state{relay=#relay{warning_msg=false}}=S) ->
    {ok, S};
handle_event({warning_msg,_,_}=E, #state{subr_id=SubscriberId}=S) ->
    {ok = smokealarm:report_event(SubscriberId, E), S};
handle_event({warning_report,_,_}, #state{relay=#relay{warning_report=false}}=S)
    ->
    {ok, S};
handle_event({warning_report,_,About}=E, #state{subr_id=SubscriberId}=S) ->
    case About of
        {_,std_warning,[{?APP,_}|_]} -> ignore;  % sa_debug.hrl ?warning macros
        _ -> ok = smokealarm:report_event(SubscriberId, E)
    end,
    {ok, S};
handle_event({error,_,_}, #state{relay=#relay{error=false}}=S) ->
    {ok, S};
handle_event({error,_,_}=E, #state{subr_id=SubscriberId}=S) ->
    {ok = smokealarm:report_event(SubscriberId, E), S};
handle_event({error_report,_,_}, #state{relay=#relay{error_report=false}}=S) ->
    {ok, S};
handle_event({error_report,_,{CrashedP,crash_report,[H|_]}}=E, #state{}=S0) ->
    UncachedTopSASupP = whereis(sa_sup),
    S1 = S0#state{top_sa_sup_pid=UncachedTopSASupP},
    IsSAP = case UncachedTopSASupP of
                undefined ->
                    false;
                CrashedP ->
                    true;
                _ -> % Ignore if crashed process is part of SA sup tree
                    case proplists:get_value(ancestors, H) of
                        L when is_list(L) ->
                            lists:member(UncachedTopSASupP, L);
                        _ ->
                            false
                    end
            end,
    if
        IsSAP -> {ok, S1};
        true -> {ok = smokealarm:report_event(S0#state.subr_id, E), S1}
    end;
handle_event({error_report,_,_}=E, #state{subr_id=SubscriberId}=S) ->
    {ok = smokealarm:report_event(SubscriberId, E), S};
handle_event({P,_,_}, #state{relay=#relay{error_report=true}}=S) when is_pid(P)
    ->
    {ok, S};
handle_event({P,_,_}=E, #state{subr_id=SubscriberId}=S) when is_pid(P) ->
    {ok = smokealarm:report_event(SubscriberId, E), S}.
   
handle_call(_, #state{}=S) ->
    {ok, S}.

handle_info(_, #state{}=S) ->
    {ok, S}.
    
terminate(_, #state{}=S) ->
    S.
    
code_change(_, #state{}=S, _) ->
    {ok, S}.

%%% Helpers --------------------------------------------------------------------

to_relay_rec([]) ->
    to_relay_rec([info_msg, info_report, warning_msg, warning_report, error,
                  error_report]);
to_relay_rec(RelayL0) when is_list(RelayL0) ->
    KnownL = ?PROPLIST_FROM_RECORD(#relay{}, relay),
    case sa_utils:proplists_subtract(RelayL0, KnownL) of
        [] ->
            RelayL1 = [chk_relay_item(X) ||
                        X <- sa_utils:proplists_umerge(RelayL0, KnownL)], 
            ?PROPLIST_TO_RECORD(RelayL1, relay);
        Unknown ->
            throw({'unknown_event', Unknown})
    end;
to_relay_rec(Bad) ->
    throw({'bad_opt', Bad}).

chk_relay_item({Event, undefined}) ->
    {Event, false};
chk_relay_item({_, V}=Good) when is_boolean(V) ->
    Good;
chk_relay_item(Bad) ->
    throw({'bad_event', Bad}).
