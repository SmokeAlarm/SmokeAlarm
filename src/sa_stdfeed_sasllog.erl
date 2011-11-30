%%%-----------------------------------------------------------------------------
%%%
%%% Desc:       Predefined SmokeAlarm error_logger feed 
%%%             
%%% Purpose:    - Tell a SmokeAlarm detector about applications started/stopped,
%%%               supervisors started/stopped and supervisor children
%%%               started/stoppped/(non-sa)crashed by relaying specific events
%%%               from error_logger.
%%%             - Provide events similar to sa_stdfeed_errlog but more specific
%%%               for OTP applciations. i.e. compared to errlog feed, this feed
%%%               should...
%%%                 1. Only relay application+supervisor related events
%%%                    (non app/sup info/error/warnings are ignored)
%%%                 2. Provide convenient event names for 1.
%%%                 3. Provide options for declaring which apps/sups to relay
%%%                 4. Start/reconfigure/restart SASL if necessary
%%%             - Provide a more "responsive" alternative to the subset of
%%%               sa_stdfeed_appmoni feed which also deal with app/sup events
%%%               i.e. compared to appmon info feed, this feed should not
%%%                    be poll-based but should be as "immediate" as possible.
%%%
%%%-----------------------------------------------------------------------------

-module(sa_stdfeed_sasllog).
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
-record(opts, {apps, sups, relay}).
-record(relay, {app_started, app_exited, child_started, child_crashed}).
-record(state, {subr_id, top_sa_sup_pid, opts}).
-include("common.hrl").
                
%%% Interface ------------------------------------------------------------------

subscribe(SubscriberId, OptsL) when is_list(OptsL) ->
    OptsR = #opts{relay=#relay{}=Relay} = to_opts_rec(OptsL),
    S = #state{subr_id = SubscriberId, top_sa_sup_pid = whereis(sa_sup),
               opts = OptsR},
    {ok, _} = ensure_sasl_with_conf(Relay),
    {ok = gen_event:add_handler(error_logger, {?MODULE,SubscriberId}, S), null}.

unsubscribe(SubscriberId, null) ->
    case gen_event:delete_handler(error_logger, {?MODULE,SubscriberId}, []) of
        #state{} -> ok;
        Error -> Error
    end.

%%% 'gen_event' callbacks ------------------------------------------------------

init(#state{}=S) ->
    {ok, S}.

% Ignore App Controller reporting on SA
handle_event({info_report,_,{_,_,[{application,?APP}|_]}}, #state{}=S)
    ->
    {ok, S}; 
% Maybe report app events
handle_event({info_report,_,{_,progress,[{application,App},{started_at,_}|_]}}=I,
             #state{opts=#opts{relay=#relay{app_started=true}}=Opts}=S) 
    ->
        case undefined=:=Opts#opts.apps
             orelse sets:is_element(App, Opts#opts.apps)
        of
            false -> {ok, S};
            true -> {ok = smokealarm:report_event(S#state.subr_id,
                                                  {app_started,I}), S}      
        end;
handle_event({info_report,_,{_,std_info,[{application,App},{exited,_}|_]}}=I,
             #state{opts=#opts{relay=#relay{app_exited=true}}=Opts}=S)
    ->
        case undefined=:=Opts#opts.apps
             orelse sets:is_element(App, Opts#opts.apps)
        of
            false -> {ok, S};
            true -> {ok = smokealarm:report_event(S#state.subr_id,
                                                 {app_exited,I}), S}
        end;
% Maybe report sup events
handle_event({info_report,_,{_,progress,[{supervisor,Sup},{started,_}|_]}}=I,
             #state{opts=#opts{relay=#relay{child_started=true}}=Opts}=S)
    ->
        case undefined=:=Opts#opts.sups
             orelse sets:is_element(Sup, Opts#opts.sups)
        of
            false -> {ok, S};
            true -> {ok = smokealarm:report_event(S#state.subr_id,
                                                  {child_started,I}), S}
        end;
% Ignore owner detector crashes and top SA sup crashes
handle_event({error_report,_,{DetP,crash_report,_}},
             #state{subr_id={_,DetP,_}}=S)
    ->
    {ok, S}; 
handle_event({error_report,_,{TopSASupP,crash_report,_}},
             #state{top_sa_sup_pid=TopSASupP}=S) when is_pid(TopSASupP)
    ->
    {ok, S}; 
% Maybe report crash
handle_event({error_report,_,{CrashedP,crash_report,[H|_]}}=I,
             #state{opts=#opts{relay=#relay{child_crashed=true}}}=S0)
    ->
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
        true -> {ok = smokealarm:report_event(S0#state.subr_id,
                                              {child_crashed,I}), S1}
    end;
% Ignore everything else
handle_event({_,_,{P,_,_}}, #state{}=S) when is_pid(P) ->
    {ok, S};   
handle_event({P,_,_}, #state{}=S) when is_pid(P) ->
    {ok, S}.
   
handle_call(_, #state{}=S) ->
    {ok, S}.

handle_info(_, #state{}=S) ->-
    {ok, S}.
    
terminate(_, #state{}=S) ->
    S.
    
code_change(_, #state{}=S, _) ->
    {ok, S}.

%%% Helpers --------------------------------------------------------------------

ensure_sasl_with_conf(#relay{child_crashed=true}=Relay) when
                      Relay#relay.app_started; Relay#relay.app_exited;
                      Relay#relay.child_started
    ->
    maybe_restart_sasl(all);
ensure_sasl_with_conf(#relay{child_crashed=true}) ->
    maybe_restart_sasl(error);
ensure_sasl_with_conf(#relay{}=Relay) when
                      Relay#relay.app_started; Relay#relay.app_exited;
                      Relay#relay.child_started
    ->
    maybe_restart_sasl(progress);
ensure_sasl_with_conf(#relay{}) ->
    start_sasl_if_not_started().
    
start_sasl_if_not_started() ->
    case application:start(sasl) of
        ok ->
            {ok, started};
        {error,{already_started,sasl}} ->
            {ok, already_started}
    end.
    
maybe_restart_sasl(RequiredLogType) ->
    case application:load(sasl) of
        ok -> ok;
        {error, {already_loaded,sasl}} -> ok
    end,
    EnvDflt =   case application:get_key(sasl, env) of
                    undefined -> [];
                    {ok, L} when is_list(L)-> L
                end,
    EnvCurr = lists:keymerge(1, lists:keysort(1, application:get_all_env(sasl)),
                                lists:keysort(1, EnvDflt)),
    case lists:keystore(errlog_type, 1, EnvCurr,
                        {errlog_type,RequiredLogType})
    of
        EnvCurr ->
            start_sasl_if_not_started();
        EnvUpd ->
            Res =   case application:stop(sasl) of
                        ok -> restarted;
                        {error, {not_started,sasl}} -> started
                    end,
            lists:foreach(
                fun({K,V}) -> ok=application:set_env(sasl, K, V) end, EnvUpd),
            {ok = application:start(sasl), Res}
    end.
    
to_opts_rec(OptsL0) when is_list(OptsL0)->
    KnownL = ?PROPLIST_FROM_RECORD(#opts{}, opts),
    case sa_utils:proplists_subtract(OptsL0, KnownL) of
        [] ->
            OptsL1 = [chk_opt(X) ||
                        X <- sa_utils:proplists_umerge(OptsL0, KnownL)],
            ?PROPLIST_TO_RECORD(OptsL1, opts);
        Unknown ->
            throw({'unknown_opts', Unknown})
    end.

chk_opt({apps, undefined}=Good) ->
    Good;
chk_opt({apps, []}) ->
    {apps, undefined};
chk_opt({apps, [_|_]=V0}) ->
    {apps, sets:from_list(V0)};
chk_opt({sups, undefined}=Good) ->
    Good;
chk_opt({sups, []}) ->
    {sups, undefined};
chk_opt({sups, [_|_]=V0}) ->
    {sups, sets:from_list(V0)};
chk_opt({relay, undefined}) ->
    {relay, to_relay_rec([])};
chk_opt({relay, V0}) when is_list(V0) ->
    {relay, to_relay_rec(V0)};
chk_opt(Bad) ->
    throw({'bad_opt', Bad}).

to_relay_rec([]) ->
    to_relay_rec([app_started, app_exited, child_started, child_crashed]);
to_relay_rec(RelayL0) when is_list(RelayL0) ->
    KnownL = ?PROPLIST_FROM_RECORD(#relay{}, relay),
    case sa_utils:proplists_subtract(RelayL0, KnownL) of
        [] ->
            RelayL1 = [chk_relay_item(X) ||
                        X <- sa_utils:proplists_umerge(RelayL0, KnownL)], 
            ?PROPLIST_TO_RECORD(RelayL1, relay);
        Unknown ->
            throw({'unknown_event', Unknown})
    end.

chk_relay_item({Event, undefined}) ->
    {Event, false};
chk_relay_item({_, V}=Good) when is_boolean(V) ->
    Good;
chk_relay_item(Bad) ->
    throw({'bad_event', Bad}).
