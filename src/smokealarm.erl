%%%-----------------------------------------------------------------------------
%%%
%%% Desc:       Main public module for SmokeAlarm.
%%%
%%% Purpose:    - Provides API to other applications/shell.
%%%             - Implements application behaviour.
%%%
%%%-----------------------------------------------------------------------------

-module(smokealarm).
-legal("Copyright (c) 2011, The Authors.\n"
        "http://github.com/SmokeAlarm\n"
        "See LICENSE.txt for licensing information.").
-author("Edmond Begumisa <ebegumisa at hysteria dash tech dot com>").

%% Public Control
-export([dev_start_stand_alone/0, dev_start_stand_alone/1,
         dev_stop_stand_alone/0]).

%% Public Interface
-export([get_beams/0, get_modules/0,
         enable_trace_me/1, enable_trace_me/2]).
-export([start_detector/1, start_detector/2, start_detector/3, start_detector/4,
         stop_detector/1]).
-export([report_event/2, report_event/3,
         report_multi_events/2,
         report_multi_simul_events/2, report_multi_simul_events/3]).

%% Private Interface
-behaviour(application).
-export([start/2, stop/1]).

%% Private Preprocess
-define(TRACER_DEFAULT_PORT, 9999).
-define(TRACE_ME_DETAIL, 0).
-include("common.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-record(app_state, {stand_alone = false}).

%%% Control --------------------------------------------------------------------

%% Start/stop directly without appliction controller (i.e. don't use .app file)
%% NOTE: These are intended for testing and development only.

dev_start_stand_alone() ->
    dev_start_stand_alone([]).
    
dev_start_stand_alone(StartArgs) ->
    start(normal, StartArgs, #app_state{stand_alone = true}).
    
dev_stop_stand_alone() ->
    stop(#app_state{stand_alone = true}).

%%% Interface ------------------------------------------------------------------

%% Get a list of paths for beams that belong to the smokealarm application.
get_beams() ->
    EbinDir = filename:dirname(code:where_is_file(?MODULE_STRING ".app")),
    filelib:wildcard(filename:join(EbinDir, "*.beam")).

%% Get a list of modules (atoms) that belong to the smokealarm application.
%% Useful for tracing.
get_modules() ->
    [list_to_atom(filename:basename(Path, ".beam")) || Path <- get_beams()].

%% Turn on/off "high-level" tracing for the smokealarm application (enable
%% sa_debug.hrl ?trace_me macros). These are for development and debugging.
%%      i.e.  - Start a tracer if one not already started
%%            - Trace calls to et:trace_me where calling module belongs to the
%%              smokealarm application.
%% See: ../include/smokealarm_conf.hrl #trace_conf{} for more.
enable_trace_me(true) ->
    enable_trace_me(true, ?TRACER_DEFAULT_PORT);
enable_trace_me(false) ->
    sa_debug:enable_mods_trace(get_modules(), trace_me, false).
    
enable_trace_me(true, TracerType) ->
    case sa_debug:start_tracer(TracerType) of
        {ok, Pid} ->
            ?info('tracing', "Started tracer ~p.", [Pid], TracerType);
        {error, already_started} ->
            ok
    end,
    sa_debug:enable_mods_trace(get_modules(), trace_me, true);
enable_trace_me(false, _TracerType) ->
    sa_debug:enable_mods_trace(get_modules(), trace_me, false).
    
start_detector(Key) ->
    start_detector(Key, {[{match, fun(E,O,undefined) ->
                                    {trigger,undefined,{E,O}}
                                  end}],
                         retrip,
                         undefined}).   

start_detector(Key, Opts) when is_list(Opts)->
    InitFeeds = proplists:get_value(feeds, Opts, []),
    InitReactors = proplists:get_value(reactors, Opts, []),
    case Triggers = proplists:get_value(triggers, Opts, undefined) of
        undefined ->
            Why = {triggers,undefined},
            {error, {Why,{by_opts,Opts}}};
        _ ->
            case start_detector(Key, Triggers, InitFeeds, InitReactors) of
                {ok, _}=Ret ->
                    Ret;
                {error, Why} ->
                    {error, {Why,{by_opts,Opts}}}
            end
    end;
start_detector(Key, Triggers) ->
    start_detector(Key, Triggers, [], []).

start_detector(Key, Triggers, InitFeeds) ->
    start_detector(Key, Triggers, InitFeeds, []).

start_detector(Key, {TripTrigger,ResetTrigger,InitRes}, InitFeeds, InitReactors)
    ->
    GetESP =    fun (X) ->
                    case sa_esp:is_esp(X) of
                        true  ->
                            {ok, X};
                        false ->
                            try {ok, sa_esp:new(X)}
                            catch _:Why -> {error, Why}
                            end
                    end
                end,
    TripReply = {TripWhat,TripESP} = GetESP(TripTrigger),
    ResetReply = {ResetWhat,ResetESP} =  case ResetTrigger of
                                once -> {ok,once};
                                retrip -> {ok,retrip};
                                _ -> GetESP(ResetTrigger)
                            end,
    if  % Try and give more detailed info on bad options than just pattern match error 
        TripWhat=/=ok andalso ResetWhat=/=ok ->
            {error, [{TripReply,{by_opt,TripTrigger}},
                     {ResetReply,{by_opt,ResetTrigger}}]};
        TripWhat=/=ok ->
            {error, {TripReply,{by_opt,TripTrigger}}};
        ResetWhat=/=ok ->
            {error, {ResetReply,{by_opt,ResetTrigger}}};
        not is_list(InitFeeds) ->
            {error, {badarg,InitFeeds}};
        not is_list(InitReactors) ->
            {error, {badarg,InitReactors}};
        true ->
            case sa_detectors_sup:start_child(
                    Key, {TripESP,ResetESP,InitRes}, InitFeeds, InitReactors)
            of
                {ok, Pid} -> {ok, _DetectorId={Key,Pid}};
                Error -> Error
            end
    end;
start_detector(_, Invalid, _, _) ->
    {error, {badarg,Invalid}}.

stop_detector({_Key,_Pid}=DetectorId) ->
    sa_detector:stop(DetectorId).
    
%% Report an event to a detector optionally with an origin or part of an origin.
%% * The SubscriberId {DetectorId, DetectorPid, FeedKey} tells SmokeAlarm where
%%   to send the event for processing. It should have been passed to your feed
%%   when it was added to a detector.
%% * The origin {Node,Pid,TimeStamp} enables reporting of an event that occured
%%   earlier and/or somewhere else after-the-fact.
%%   - If unspecified (i.e. if report_event/2 is used)...
%%          e.g. report_event(MyFeedId, MyEvent).
%%     ... then then origin defaults to {node(), self(), now()}
%%   - If a timestamp of same format as erlang:now() is passed...
%%          e.g. report_event(MyFeedId, MyEvent, {{1318,471585,778000}).
%%     ... then the node and pid are defaulted {node(), self(), YourTS}
%%   - If a pid is passed...
%%          e.g. report_event(MyFeedId, MyEvent, <0.530.0>}
%%     ... then the node and timestamp are defaulted {node(), YourPid, now()}
%%   - Otherwise a full origin is expected...
%%          e.g. report_event(MyFeedId, MyEvent, {EventNode, EventPid, EventTS}}

report_event(SubscriberId, Event) ->
    report_event(SubscriberId, Event, {node(),self(),now()}).

report_event({_DetectorId,_DetectorPid,_FeedKey}=SubscriberId, Event,
             {Nde,Pid,{MegaSecs,Secs,MicrosSecs}}=Origin) when
                is_atom(Nde), is_pid(Pid),
                is_number(MegaSecs), is_number(Secs), is_number(MicrosSecs)
    ->
    sa_detector:report_event(SubscriberId, Event, Origin);
report_event(SubscriberId, Event, {MegaSecs,Secs,MicrosSecs}=OriginTimeStamp)
        when is_number(MegaSecs), is_number(Secs), is_number(MicrosSecs)
    ->
    report_event(SubscriberId, Event, {node(),self(),OriginTimeStamp});
report_event(SubscriberId, Event, OriginPid) when is_pid(OriginPid) ->
    report_event(SubscriberId, Event, {node(),OriginPid,now()}).

report_multi_events(SubscriberId, EventOriginTupList) when
        is_list(EventOriginTupList)->
    sa_detector:report_multi_events(SubscriberId, EventOriginTupList).

report_multi_simul_events(SubscriberId, Events) ->
    report_multi_simul_events(SubscriberId, Events, {node(),self(),now()}).

report_multi_simul_events(
    {_DetectorId,_DetectorPid,_FeedKey}=SubscriberId, Events,
    {Nde,Pid,{MegaSecs,Secs,MicrosSecs}}=Origin) when
        is_list(Events),
        is_atom(Nde), is_pid(Pid),
        is_number(MegaSecs), is_number(Secs), is_number(MicrosSecs)
    ->
    sa_detector:report_multi_simul_events(SubscriberId, Events, Origin);
report_multi_simul_events(SubscriberId, Events, {_,_,_}=OriginTimeStamp) ->
    report_multi_simul_events(
        SubscriberId, Events, {node(),self(),OriginTimeStamp});
report_multi_simul_events(SubscriberId, Events, OriginPid) ->
    report_multi_simul_events(SubscriberId, Events, {node(),OriginPid,now()}).


%%% 'application' callbacks ----------------------------------------------------

start(normal, StartArgs) ->
    start(normal, StartArgs, #app_state{stand_alone=false}).
    
start(normal, [], AppState) ->
    start(normal, [[], []], AppState);
start(normal, [AppConf], AppState) ->
    start(normal, [AppConf, []], AppState);
start(normal, [#app_conf{}=AppConfR, TraceConf], AppState) ->
    start(normal, [?PROPLIST_FROM_RECORD(AppConfR, app_conf), TraceConf],
          AppState);
start(normal, [AppConf, #trace_conf{}=TraceConfR], AppState) ->
    start(normal, [AppConf, ?PROPLIST_FROM_RECORD(TraceConfR, trace_conf)],
          AppState);
start(normal, [AppConfL, TraceConfL],
      #app_state{stand_alone=StandAlone}=AppState)
    ->
    AppConfR = to_appconf_rec(AppConfL),
    TraceConfR = to_traceconf_rec(TraceConfL),
    F = case StandAlone of 
            true -> dev_start_stand_alone;
            false -> start_link
        end,
    case sa_sup:F(AppConfR, TraceConfR) of
        {ok, Pid} -> {ok, Pid, AppState};
        Error ->     Error
    end.

stop(#app_state{stand_alone=false}) -> % We were started from the application controller
    ok;
stop(#app_state{stand_alone=true}) -> % We were started directly
    ?info('application', "Stop stand_alone."),
    sa_sup:dev_stop_stand_alone(),
    ok.

%%% Helpers --------------------------------------------------------------------

%% Fill in missing keys with with record defaults, convert result to record.

to_appconf_rec(AppConfL0) ->
    DefltL = ?PROPLIST_FROM_RECORD(#app_conf{}, app_conf),
    AppConfL1 = sa_utils:proplists_umerge(AppConfL0, DefltL),
    ?PROPLIST_TO_RECORD(AppConfL1, app_conf).
    
to_traceconf_rec(TraceConfL0) ->
    DefltL = ?PROPLIST_FROM_RECORD(#trace_conf{}, trace_conf),
    TraceConfL1 = sa_utils:proplists_umerge(TraceConfL0, DefltL),
    ?PROPLIST_TO_RECORD(TraceConfL1, trace_conf).
