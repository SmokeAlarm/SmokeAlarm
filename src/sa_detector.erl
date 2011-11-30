%%%-----------------------------------------------------------------------------
%%%
%%% Desc:       Worker process for event detection and reaction.
%%%
%%% Purpose:    - Subscribe to one or more feeds, accept events from them, pass
%%%               events to an ESP, then trigger one or more reactors if the
%%%               ESP trips.
%%%             - Automatically unsubscribe from feeds when quitting.
%%%
%%% Note:       This module is defensively programmed in many places because
%%%             detectors are "kernels" that shouldn't be crashed by
%%%             feeds/reactors/requests, and when they do crash they should know
%%%             exactly why. That is: we avoid "letting a detector crash", and
%%%             remove/crash the faulty feeds/reactors/requesters instead.
%%%
%%% Internals:  - The detector has only two finite states - reset and tripped.
%%%                    Two ESPs determine whether the detector should flip
%%%               between these states: the Trip ESP for reset->tripped and the
%%%               Reset ESP for tripped->reset.
%%%                    Both ESPs have a heirarchy substates (corresponding to
%%%               an ESP statement) that flip internally as events come in but
%%%               only flip the detector's state when a complete path (root to
%%%               leaf) of substates have been flipped.
%%%                    Only *one* ESP is active at one time (i.e. is passed
%%%               events). When the detector is in the reset state, the trip
%%%               ESP is is active, when in the tripped state, either the reset
%%%               ESP is active or the trip ESP remains active depending on the
%%%               detection mode (see below.)
%%%                    When the active ESP flips the detector's state, the
%%%               corresponding callbacks for all reactors are called with the
%%%               result of the ESP statement (e.g. when the trip ESP flips the
%%%               detector from reset->tripped, all tripped_cb functions for
%%%               each reactor are called with the ESP statement return value).
%%%             - A detector can be in spawned in one of three detection modes:
%%%               trip-once, retrip and trip-reset.
%%%                    In trip-once mode: there is no reset ESP (it is set to
%%%               the 'once' atom.) Events are passed to the trip ESP until it
%%%               flips the detector to the tripped state then all the tripped
%%%               callbacks for all reactors are made. The detector process
%%%               then exits (normal). No reactor reset callbacks are made.
%%%                    In retrip mode: there is also no reset ESP (it is set to
%%%               the 'retrip' atom.) Events are passed to the trip ESP until
%%%               it flips the detector just like above. However, after the
%%%               tripped callbacks are made, new events are passed back to the
%%%               same trip ESP (the ESP substate is not cleared - it continues
%%%               exactly from where it stopped so it's able to trip other
%%%               matched events that were partially in the pipeline.)
%%%                    In trip-reset mode: both a tripped ESP and a reset ESP
%%%               are defined. Events are first passed to the trip ESP until it
%%%               flips the detector just like above. However, after the
%%%               tripped callbacks are made, the reset ESP bacomes active. New
%%%               events then are passed to a new reset ESP with fresh substate
%%%               until it flips the detector back to reset, upon which all the
%%%               reset callbacks for all reactors are made. On switching back
%%%               to reset, new events will be passed to a new tripped ESP with
%%%               fresh substate. So unlike retrip mode, trip-reset mode will
%%%               starting detecting afresh after reset (events ARE queued-up
%%%               via the process mailbox). 
%%%             
%%%-----------------------------------------------------------------------------

-module(sa_detector).
-legal("Copyright (c) 2011, The Authors.\n"
        "http://github.com/SmokeAlarm\n"
        "See LICENSE.txt for licensing information.").
-author("Edmond Begumisa <ebegumisa at hysteria dash tech dot com>").

%% Public Control
-export([stop/1]).
-export([dev_start_stand_alone/1, dev_start_stand_alone/2,
         dev_start_stand_alone/3,
         dev_stop_stand_alone/1]).

%% Public Interface
-export([add_feed/3, add_feeds/2,
         remove_feed/2, remove_feeds/2,
         remove_all_feeds/1,
         which_feeds/1]).
-export([add_reactor/3, add_reactors/2, add_reactors_atomic/2,
         remove_reactor/2, remove_reactors/2, remove_reactors_atomic/2,
         remove_all_reactors/1,
         which_reactors/1]).
-export([report_event/3, report_multi_events/2, report_multi_simul_events/3]).

-export([dev_inspect/1]).

%% Private Control
-export([start_link/7]).

%% Private Interface
-behaviour(gen_fsm).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).
-export([reset/2, tripped/2]).

%% Private Preprocess
-define(TRACE_ME_DETAIL, 20). % Above detector's sup, below feeds and reactors.
-include("common.hrl").
-record(data, {
    key,            % term() -- Key used to register with the supervisor
    faulty_freq,    % {MaxR,MaxTu} NB: MaxTu is in microseconds
    replay_handle,  % undefined|et:colleter_pid()|et:record(table_handle) --
                    % this affects ?report_me macros (see debug.hrl.) i.e. for
                    % the "replay" feature...
                    %    - If the "replay" feature is enabled "major" markers
                    %      are sent to et regardless of whether tracing is
                    %      swtiched on (unlike trace_me macros that require
                    %      tracing).
                    %    - If the "replay" feature is disabled, (i.e.
                    %      replay_handle is 'undefined'), ?report_me macros work
                    %      like ?trace_me macros (i.e. only if tracing is on)
    esp_acc,        % term() -- Current result value of active ESP
    new_trip_esp,   % sa_esp() -- Cached new trip ESP with fresh state (so we
                    %   don't have to keep calling sa_esp:new)
    trip_esp,       % sa_esp() -- Current active/inactive trip ESP
    new_reset_esp,  % once|retrip|sa_esp() -- Depends on detection mode:
                    %   - In trip-once or retrip mode, the correspodning atom.
                    %   - In trip-reset mode, a cached new reset ESP with fresh
                    %     state (so we don't have to keep calling sa_esp:new)
    reset_esp,      % once|retrip|sa_esp:new() -- Depends on detection mode:
                    %   In trip-once or retrip mode, the correspodning atom
                    %   In trip-reset mode, current active/inactive reset
    feeds,          % sa_detector_feeds() -- Subscribed feeds
    reactors        % sa_detector_reactors() -- Active triggerable reactors
}).
        
%%% Control --------------------------------------------------------------------

start_link(OTPStartOpts, {_MxR,_MxTu}=FaultyFreq, Key,
           {_Trip,_Reset,_InitResult}=ESPs, InitFeeds, InitReactors,
           ReplayHandle) 
    ->
    InitByP = self(),
    ?trace_me(sa, Key, "Start", [{'by',InitByP}, {'init_feeds',InitFeeds},
                                 {'init_reactors',InitReactors},
                                 {'faulty_freq',FaultyFreq},{'esps',ESPs},
                                 {'replay_handle',ReplayHandle}]),
    gen_fsm:start_link(?MODULE, {Key,InitByP,FaultyFreq,ESPs,
                                 InitFeeds,InitReactors,ReplayHandle},
                        OTPStartOpts).

stop({Key,Pid}=_DetectorId) ->
    gen_fsm:send_all_state_event(Pid, {{?MODULE,Key}, ?FUNCTION, self()}).

%% Start/stop directly without a supervisor.
%% These are intended for *internal* SA testing and development only.

dev_start_stand_alone(Key) ->
    dev_start_stand_alone(Key, [], []).

dev_start_stand_alone(Key, Opts) ->
    dev_start_stand_alone(Key, Opts, []).

dev_start_stand_alone(Key, Opts, OTPStartOpts) when is_list(Opts) ->
    DfltAppConf = #app_conf{},
    DfltFaultyFreq = {DfltAppConf#app_conf.dtor_faulty_freq_maxr,
                      DfltAppConf#app_conf.dtor_faulty_freq_maxtu},
    Ret = {ok,Pid} =
        start_link(OTPStartOpts,
                   proplists:get_value('faulty_freq', Opts, DfltFaultyFreq),
                   Key,
                   proplists:get_value('esps', Opts, []),
                   proplists:get_value('init_feeds', Opts, []),
                   proplists:get_value('init_reactors', Opts, []),
                   proplists:get_value('replay_handle', Opts, undefined)),
    unlink(Pid),
    Ret.

dev_stop_stand_alone(DetectorId) ->
    stop(DetectorId).

%%% Interface ------------------------------------------------------------------

%%% See corresponding smokealarm.erl functions for descriptions.
%%%
%%% NOTE: These being non-registered processes, all message-related calls to
%%%       gen_fsm (i.e. gen_fsm:*_send_*) are tagged with {?MODULE,DetKey} for
%%%       easier debugging since this is usually more informative when bad
%%%       gen_fsm requests fail to find a process or fail to match a handler.
%%%       THEREFORE:       
%%%       1. Correspodning handle_* functions shouldn't rely on the
%%%          {?MODULE,DetKey} tag's DetKey element but should match the DetKey
%%%          with the internal key while ignoring the module element. This
%%%          double checks that the caller is calling the detector she thinks
%%%          she's calling.
%%%       2. Callers pass a detector id {DetKey,DetPid} as the first argument
%%%          instead of a plain pid normally done with non-registered OTP
%%%          processes. A tagged pid is usually more informative when debugging
%%%          the caller's side.

add_feed({Key,Pid}=_DetectorId, FeedKey, FeedSpec) ->
    gen_fsm:sync_send_all_state_event(Pid,
        {{?MODULE,Key}, {sa_detector_feeds,?FUNCTION}, {FeedKey,FeedSpec}}).
        
add_feeds({Key,Pid}, FeedKeySpecPairList)  ->
    gen_fsm:sync_send_all_state_event(Pid,
        {{?MODULE,Key}, {sa_detector_feeds,?FUNCTION}, FeedKeySpecPairList}).

remove_feed({Key,Pid}, FeedKey) ->
    gen_fsm:sync_send_all_state_event(Pid,
        {{?MODULE,Key}, {sa_detector_feeds,?FUNCTION}, FeedKey}).

remove_feeds({Key,Pid}, FeedKeyList) ->
    gen_fsm:sync_send_all_state_event(Pid,
        {{?MODULE,Key}, {sa_detector_feeds,?FUNCTION}, FeedKeyList}).

remove_all_feeds({Key,Pid}) ->
    gen_fsm:sync_send_all_state_event(Pid,
        {{?MODULE,Key}, {sa_detector_feeds,?FUNCTION}, empty}).

which_feeds({Key,Pid}) ->
    gen_fsm:sync_send_all_state_event(Pid,
        {{?MODULE,Key}, {sa_detector_feeds,?FUNCTION}, empty}).

add_reactor({Key,Pid}=_DetectorId, ReactorKey, ReactorSpec) ->
    gen_fsm:sync_send_all_state_event(Pid,
        {{?MODULE,Key}, {sa_detector_reactors,?FUNCTION},
         {ReactorKey,ReactorSpec}}).
        
add_reactors({Key,Pid}, ReactorKeySpecPairList)  ->
    gen_fsm:sync_send_all_state_event(Pid,
        {{?MODULE,Key}, {sa_detector_reactors,?FUNCTION},
         ReactorKeySpecPairList}).

add_reactors_atomic({Key,Pid}, ReactorKeySpecPairList)  ->
    gen_fsm:sync_send_all_state_event(Pid,
        {{?MODULE,Key}, {sa_detector_reactors,?FUNCTION},
         ReactorKeySpecPairList}).

remove_reactor({Key,Pid}, ReactorKey) ->
    gen_fsm:sync_send_all_state_event(Pid,
        {{?MODULE,Key}, {sa_detector_reactors,?FUNCTION}, ReactorKey}).

remove_reactors({Key,Pid}, ReactorKeyList) ->
    gen_fsm:sync_send_all_state_event(Pid,
        {{?MODULE,Key}, {sa_detector_reactors,?FUNCTION}, ReactorKeyList}).

remove_reactors_atomic({Key,Pid}, ReactorKeyList) ->
    gen_fsm:sync_send_all_state_event(Pid,
        {{?MODULE,Key}, {sa_detector_reactors,?FUNCTION}, ReactorKeyList}).

remove_all_reactors({Key,Pid}) ->
    gen_fsm:sync_send_all_state_event(Pid,
        {{?MODULE,Key}, {sa_detector_reactors,?FUNCTION}, empty}).
        
which_reactors({Key,Pid}) ->
    gen_fsm:sync_send_all_state_event(Pid,
        {{?MODULE,Key}, {sa_detector_reactors,?FUNCTION}, empty}).

report_event({Key,Pid,FeedKey}=_SubscriberId, Event, Origin) ->
    gen_fsm:send_event(Pid, {{?MODULE,Key}, FeedKey, Event, Origin}).
    
report_multi_events({Key,Pid,FeedKey}, EventsOriginTupList) ->
    gen_fsm:send_event(Pid, {{?MODULE,Key}, FeedKey, EventsOriginTupList}).
    
report_multi_simul_events({Key,Pid,FeedKey}, Events, Origin) ->
    gen_fsm:send_event(Pid, {{?MODULE,Key}, FeedKey, {Events, Origin}}).
    
dev_inspect({_,Pid}) ->
    dev_inspect(Pid);
dev_inspect(Pid) ->
    gen_fsm:sync_send_all_state_event(Pid, ?FUNCTION).
    
%%% 'gen_fsm' callbacks --------------------------------------------------------

init({K, InitByP, FaultyFreq, {TripESP,ResetESP,ESPAcc}, InitFeeds,
     InitReactors, ReplayHandle}) when
        is_list(InitFeeds), is_list(InitReactors)
    ->
    ?trace_me(K, K, "Starting", [{'pid',self()}, {'by',InitByP}]),
    put(detector_key, K),
    process_flag(trap_exit, true),
    SchedL = [{fun do_init_reactors/3, InitReactors}, % Order important (add reactors before feeds)
              {fun do_init_feeds/3, InitFeeds}],
    ok = gen_fsm:send_all_state_event(self(), {{?MODULE,K},schedule,SchedL,K}),
    D0 = #data{key = K, faulty_freq = FaultyFreq, replay_handle=ReplayHandle,
               feeds = sa_detector_feeds:new(),
               reactors = sa_detector_reactors:new(),
               esp_acc = ESPAcc,
               new_trip_esp = TripESP, trip_esp = TripESP,
               new_reset_esp = ResetESP, reset_esp = ResetESP},
    ReplayCont = ?report_me(ReplayHandle, K, K, "Started: reset", D0),
    {ok, reset, D0#data{replay_handle=ReplayCont}}.

do_init_reactors([], S, #data{}=D) ->
    {next_state, S, D};
do_init_reactors(InitReactorsL, S, #data{key=K,reactors=Rtors0}=D0) ->
    {{What,_}=Reply,Rtors1} =
        sa_detector_reactors:add_reactors(InitReactorsL, sa, K, Rtors0),
    ?warning_if(What=/=ok, 'detector',
                "Detector ~p init: Error adding reactors.", [K], Reply),
    {next_state, S, D0#data{reactors=Rtors1}}.

do_init_feeds([], S, #data{}=D) ->
    {next_state, S, #data{}=D};
do_init_feeds(InitFeedsL, S, #data{key=K,feeds=Feeds0}=D0) ->
    {{What,_}=Reply,Feeds1} =
        sa_detector_feeds:add_feeds(InitFeedsL, sa, K, Feeds0),
    ?warning_if(What=/=ok, 'detector',
                "Detector ~p init: Error subscribing to feeds.",[K], Reply),
    {next_state, S, D0#data{feeds=Feeds1}}.

reset({{_,K}, FeedK, E, {Nde,Pid,{MegaSecs,Secs,MicrosSecs}}=O},
      #data{key=K,esp_acc=ESPAcc0,trip_esp=TripESP0,replay_handle=Replay0}=D0)
      when is_atom(Nde), is_pid(Pid),
           is_number(MegaSecs), is_number(Secs), is_number(MicrosSecs)
    ->
    ReplayCont = ?report_me(Replay0, FeedK, K, {'event',Pid},
                            [{'dtor_state',?FUNCTION}, {'feed',FeedK},
                             {'origin',O}, {'event',E},
                             {'esp_acc',ESPAcc0}, {'active_esp',TripESP0}]),
    {Val, ESPAcc1, TripESP1} = sa_esp:eval_event(E, O, ESPAcc0, TripESP0),
    {Next, S2, D2} = 
        case Val of
            cont ->
                D1 = D0#data{esp_acc = ESPAcc1, trip_esp = TripESP1,
                             replay_handle = ReplayCont},
                {next_state, ?FUNCTION, D1};
            {trigger,ESPRes} ->
                _ = ?report_me(ReplayCont, K, K, "ESP: tripping",
                                [{acc,ESPAcc1}, {res,ESPRes}]),
                S1 = tripped,
                {_, Rtors1} =
                    sa_detector_reactors:call_all_reactors(
                            {S1,ESPRes,D0#data.faulty_freq,ReplayCont},
                            K, K, D0#data.reactors),
                D1 = D0#data{esp_acc = ESPAcc1, trip_esp = TripESP1,
                             reactors = Rtors1, replay_handle = ReplayCont},
                case D1#data.reset_esp of
                    once -> {stop,normal,D1};
                    _ -> {next_state,S1,D1}
                end
        end,
    _ = ?report_me_if(Next=:=next_state andalso ?FUNCTION=/=S2, ReplayCont,
                      K, K, {'flip',S2}),
    {Next, S2, D2};
reset({{_,K}=CTag, FeedK, {SimulEL,{Nde,Pid,{MegaSecs,Secs,MicrosSecs}}=O}},
      #data{key=K}=D0) when
        is_list(SimulEL),
        is_atom(Nde), is_pid(Pid),
        is_number(MegaSecs), is_number(Secs), is_number(MicrosSecs)
    ->
    ?trace_me(FeedK, K, 'multi_simul_events',
              [{'dtor_state',?FUNCTION}, {'feed',FeedK}, {'events',SimulEL},
               {'origin',O}]),
    Ret = case do_multi_simul_events(CTag, FeedK, O, SimulEL, ?FUNCTION, D0) of
            {[], {next_state,_,_}=Next} -> Next;
            {_, {stop,_,_}=Stop} -> Stop
          end,
    Ret;
reset({{_,K}=CTag, FeedK, EOL}, #data{key=K}=D0) when is_list(EOL) ->
    ?trace_me(FeedK, K, 'multi_events', [{'dtor_state',?FUNCTION},
                                         {'feed',FeedK}, {'events',EOL}]),
    Ret = case do_multi_events(CTag, FeedK, EOL, ?FUNCTION, D0) of
            {[], {next_state,_,_}=Next} -> Next;
            {_, {stop,_,_}=Stop} -> Stop
          end,
    Ret.
    
do_multi_simul_events(_, _, _, [], S0, D0) ->
    {[], {next_state,S0,D0}};
do_multi_simul_events(CTag, FeedK, O, SimulEL, S0, D0) ->
    ResetSF = fun reset/2, TrippedSF = fun tripped/2,
    sa_utils:lists_drop_while_folding(
        fun (E, {next_state,S1,D1}) ->
            SF = case S1 of
                    reset -> ResetSF;
                    tripped -> TrippedSF
                 end,
            case SF({CTag,FeedK,E,O}, D1) of
                ({next_state,_,_}=Cont) ->
                    {true, Cont};
                ({stop,_,_}=Bail) ->
                    {false, Bail}
            end 
        end, {next_state,S0,D0}, SimulEL).

do_multi_events(_, _, [], S0, D0) ->
    {[], {next_state,S0,D0}};
do_multi_events(CTag, FeedK, EOL, S0, D0) ->
    ResetSF = fun reset/2, TrippedSF = fun tripped/2,
    sa_utils:lists_drop_while_folding(
        fun ({E,O}, {next_state,S1,D1}) ->
            SF = case S1 of
                    reset -> ResetSF;
                    tripped -> TrippedSF
                 end,
            case SF({CTag,FeedK,E,O}, D1) of
                ({next_state,_,_}=Cont) ->
                    {true, Cont};
                ({stop,_,_}=Bail) ->
                    {false, Bail}
            end 
        end, {next_state,S0,D0}, EOL).
    
tripped({{_,K}, FeedK, E, {Nde,Pid,{MegaSecs,Secs,MicrosSecs}}=O},
        #data{key=K, esp_acc=ESPAcc0, reset_esp=ResetESP0,
              replay_handle=Replay0}=D0) when
            is_atom(Nde), is_pid(Pid),
            is_number(MegaSecs), is_number(Secs), is_number(MicrosSecs)
    ->
    {ESP0,S1} = case ResetESP0 of
                    retrip ->
                        {D0#data.trip_esp, ?FUNCTION};
                    _ when ResetESP0=/=once ->
                        {D0#data.reset_esp, reset}
                end,
    ReplayCont = ?report_me(Replay0, FeedK, K, {'event',Pid},
                            [{'dtor_state',?FUNCTION}, {'feed',FeedK},
                             {'origin',O}, {'event',E},
                             {'esp_acc',ESPAcc0}, {'active_esp',ESP0}]),
    {Val, ESPAcc1, ESP1} = sa_esp:eval_event(E, O, ESPAcc0, ESP0),
    D1 = case Val of
            cont when ResetESP0=:=retrip ->
                D0#data{esp_acc = ESPAcc1, trip_esp = ESP1,
                        replay_handle = ReplayCont};
            cont ->
                D0#data{esp_acc = ESPAcc1, reset_esp = ESP1,
                        replay_handle = ReplayCont};
            {trigger,ESPRes} ->
                _ = ?report_me_else(ResetESP0=:=retrip, ReplayCont, K, K,
                            "ESP: retrip", [{acc,ESPAcc1}, {res,ESPRes}],
                            "ESP: resetting", [{acc,ESPAcc1}, {res,ESPRes}]),
                {_, Rtors1} =
                    sa_detector_reactors:call_all_reactors(
                                {S1,ESPRes,D0#data.faulty_freq,ReplayCont},
                                 K, K, D0#data.reactors),
                case ResetESP0 of
                    retrip ->
                        D0#data{esp_acc = ESPAcc1, trip_esp = ESP1,
                                reactors = Rtors1, replay_handle = ReplayCont};
                    _ ->
                        D0#data{esp_acc = ESPAcc1,
                                trip_esp = D0#data.new_trip_esp,
                                reset_esp = D0#data.new_reset_esp,
                                reactors = Rtors1, replay_handle = ReplayCont}
                end
         end,
    _ = ?report_me_if(?FUNCTION=/=S1, ReplayCont, K, K, {'flip',S1}),
    {next_state, S1, D1};
tripped({{_,K}=CTag, FeedK, {SimulEL,{Nde,Pid,{MegaSecs,Secs,MicrosSecs}}=O}},
        #data{key=K}=D0) when
        is_list(SimulEL),
        is_atom(Nde), is_pid(Pid),
        is_number(MegaSecs), is_number(Secs), is_number(MicrosSecs)
    ->
    ?trace_me(FeedK, K, 'multi_simul_events',
              [{'dtor_state',?FUNCTION}, {'feed',FeedK}, {'events',SimulEL},
               {'origin',O}]),
    Ret = case do_multi_simul_events(CTag, FeedK, O, SimulEL, ?FUNCTION, D0) of
            {[], {next_state,_,_}=Next} -> Next;
            {_, {stop,_,_}=Stop} -> Stop
          end,
    Ret;
tripped({{_,K}=CTag, FeedK, EOL}, #data{key=K}=D0) when is_list(EOL) ->
    ?trace_me(FeedK, K, 'multi_events', [{'dtor_state',?FUNCTION},
                                         {'feed',FeedK}, {'events',EOL}]),
    Ret = case do_multi_events(CTag, FeedK, EOL, ?FUNCTION, D0) of
            {[], {next_state,_,_}=Next} -> Next;
            {_, {stop,_,_}=Stop} -> Stop
          end,
    Ret.


%% Synchronous requests (i.e. calls)
handle_sync_event({{_,K}, {sa_detector_feeds,Req}, ReqArgs}, ReqFrm, S,
                  #data{key=K,feeds=Feeds0}=D0) 
    ->
    {Reply,Feeds1} = case ReqArgs of
                        empty -> sa_detector_feeds:Req(ReqFrm, K, Feeds0);
                        _ -> sa_detector_feeds:Req(ReqArgs, ReqFrm, K, Feeds0)
                     end,
    {reply, Reply, S, D0#data{feeds=Feeds1}};
handle_sync_event({{_,K}, {sa_detector_reactors,Req}, ReqArgs}, ReqFrm, S,
                  #data{key=K,reactors=Rtors0}=D0)
    ->
    true = Req=/=call_all_reactors, % ASSERT: This is not the way to call reactors.
    {Reply,Rtors1} =
        case ReqArgs of
            empty -> sa_detector_reactors:Req(ReqFrm, K, Rtors0);
            _ -> sa_detector_reactors:Req(ReqArgs, ReqFrm, K, Rtors0)
        end,
    case Reply of
        noreply -> {next_state, S, Rtors1};
        _ -> {reply, Reply, S, D0#data{reactors=Rtors1}}
    end;
handle_sync_event(dev_inspect, ReqFrm, S, D) ->
    ?trace_me(ReqFrm, K = if
                            is_record(D, data) -> D#data.key; 
                            true -> 'UNKNOWN'
                          end, dev_inspect),
    Reply = [{'dtor_state',S}, {'dtor_data',D}],
    ?trace_me(K, ReqFrm, dev_inspect, Reply), % Why? Coz et_viewer's detail window displays nicely!
    {reply, Reply, S, D}.
    
%% Asynchronous no-reply requests (i.e. casts)
handle_event({{_,K}, schedule, {F,A}, ReqFrm}, S, #data{key=K}=D) when
                is_function(F)
    ->
    ?trace_me(ReqFrm, K, schedule, {F,A,erlang:fun_info(F)}),
    F(A, S, D);
handle_event({{_,K}, schedule, L, ReqFrm}, S, #data{key=K}=D) when is_list(L)
    ->
    ?trace_me(ReqFrm, K, schedule, L),
    lists:foreach(
        fun({_F,_A}=Sched) ->
            ok = gen_fsm:send_all_state_event(self(),
                                              {{?MODULE,K},schedule,Sched,K})
        end, L),
    {next_state, S, D};
handle_event({{_,K}, stop=Req, ReqFrm}, _, #data{key=K,feeds=Feeds0}=D0) ->
    ?trace_me(ReqFrm, K, Req, {'by', ReqFrm}),
    throw(foobar),
    {{What,_}=Reply,Feeds1} =
        sa_detector_feeds:remove_all_feeds(K, K, Feeds0),
    case What of
        ok -> {stop, normal, D0#data{feeds=Feeds1}};
        _  -> throw({error, {'stop_failed',Reply,K}})
    end.

handle_info({'EXIT',_,normal}, S, #data{}=D) ->
    {next_state, S, D};
handle_info({'EXIT',P,Why}, S, #data{key=K}=D) ->
    ?warning('detector',
             "Detector ~p: a linked process has crashed - ignoring.", [K],
                [{'pid',P}, {'reason',Why}, {'dtor_state',S}, {'dtor_data',D}]),
    {next_state, S, D}.
    
terminate(normal, S, #data{key=K,replay_handle=Replay}) ->
    _ = ?report_me(Replay, K, K, "Terminated", [{'reason',normal},
                                                {'dtor_state',S}]);
terminate(Reason, S, #data{key=K}=D0) ->
    IsCrash =   case Reason of
                    shutdown     -> false;
                    {shutdown,_} -> false;
                    _            -> true
                end,
    ?trace_me_else(IsCrash, K, K, "CRASHING", [{'reason',Reason},
                                               {'dtor_state',S},
                                               {'dtor_data',D0}],
                            sa, K, shutdown, [{'dtor_state',S}]),
    D1 =    try
                {stop,normal,DStop} = handle_event({{?MODULE,K},stop,K}, S, D0),
                DStop
            catch _:Why ->
                ?warning('detector',
                         "Detector ~p termination: one or more errors while "
                         "unsubscribing from feed(s).", [K],
                         [{'reason',Why}, {'dtor_state',S},
                          {'dtor_data',D0}, {'was_crash',IsCrash}]),
                D0
            end,
    _ = ?report_me_else(IsCrash, D1#data.replay_handle, K, K,
                        "!!CRASHED!!", [{'reason',Reason}, {'dtor_state',S},
                                        {'dtor_data',D1}],
                        "Terminated", [{'reason',Reason}, {'dtor_state',S}]).
     
code_change(_, S, D, _) ->
    {ok, S, D}.

