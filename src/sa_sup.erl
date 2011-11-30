%%%-----------------------------------------------------------------------------
%%%
%%% Desc:       Top-level SmokeAlarm Supervisor Process
%%%
%%% Purpose:    - Setup SmokeAlarm tracing if configured
%%%             - Start viewer if configured
%%%             - Start/stop other supervisors, restart them if they crash.
%%%
%%%-----------------------------------------------------------------------------

-module(sa_sup).
-legal("Copyright (c) 2011, The Authors.\n"
        "http://github.com/SmokeAlarm\n"
        "See LICENSE.txt for licensing information.").
-author("Edmond Begumisa <ebegumisa at hysteria dash tech dot com>").

%% Public Control
-export([dev_start_stand_alone/0, dev_start_stand_alone/1,
         dev_start_stand_alone/2,
         dev_stop_stand_alone/0]).

%% Public Interface
-export([]).

%% Private Control
-export([start_link/2]).

%% Private Interface
-behaviour(supervisor).
-export([init/1]).

%% Private Preprocess
-define(TRACE_ME_DETAIL, 0). % Below everyone.
-include("common.hrl").

%%% Control --------------------------------------------------------------------

start_link(AppConf, TraceConf) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {AppConf, TraceConf}).

%% Start/stop directly without an application.
%% These are intended only for testing and development only.

dev_start_stand_alone() ->
    dev_start_stand_alone(#app_conf{}, #trace_conf{}).
    
dev_start_stand_alone(AppConf) ->
    dev_start_stand_alone(AppConf, #trace_conf{}).

dev_start_stand_alone(AppConf, TraceConf) when is_list(AppConf) ->
    dev_start_stand_alone(?PROPLIST_TO_RECORD(AppConf, app_conf), TraceConf);    
dev_start_stand_alone(AppConf, TraceConf) when is_list(TraceConf) ->
    dev_start_stand_alone(AppConf, ?PROPLIST_TO_RECORD(TraceConf, trace_conf));
dev_start_stand_alone(AppConf, TraceConf) ->
    Ret = {ok, Pid} = start_link(AppConf, TraceConf),
    unlink(Pid),
    Par = list_to_atom(?MODULE_STRING "_stand_alone_pid"),
    ok = application:set_env(kernel, Par, Pid),
    Ret.
    
dev_stop_stand_alone() ->
    Par = list_to_atom(?MODULE_STRING "_stand_alone_pid"),
    {ok, Pid} = application:get_env(kernel, Par),
    true = exit(Pid, normal),
    ok = application:unset_env(kernel, Par).

%%% Interface ------------------------------------------------------------------

%%% 'supervisor' callbacks -----------------------------------------------------

init({#app_conf{}=AppConf, #trace_conf{}=TraceConf}) ->
    % Top-Level supervisor is responsible for setting up optional tracing
    % and starting viewer
    apply_trace_conf(TraceConf),
    ?trace_me(sa, sa, "Starting", self()),
    {ok, {{one_for_one, AppConf#app_conf.child_restart_freq_maxr,
                        AppConf#app_conf.child_restart_freq_maxt},
          [{sa_detectors_sup,
                {sa_detectors_sup, start_link, [AppConf]},
                permanent, 
                infinity,
                supervisor, 
                [sa_detectors_sup]}
            % More to come
          ]}}.

%%% Helpers --------------------------------------------------------------------

apply_trace_conf(#trace_conf{start_viewer=Viewer, start_tracer=undefined}) ->
    apply_start_viewer(Viewer, undefined);
apply_trace_conf(#trace_conf{start_viewer=Viewer, start_tracer=TracerType,
                             trace_flags=TraceFlags0, enable_trace_me=TraceMe,
                             enable_trace_calls=TraceCalls})
    ->
    apply_start_tracer(TracerType),
    apply_start_viewer(Viewer, TracerType), % NB: Must start et BEFORE setting trace flags otherwise sos/sol will apply to et processes degrading performance!
    _TraceFlags1 = apply_trace_flags(TraceFlags0, TracerType),
    apply_trace_me(TraceMe, TracerType),
    apply_trace_calls(TraceCalls, TracerType).

apply_start_tracer(TracerType) ->
    case sa_debug:start_tracer(TracerType) of
        {error, already_started} ->
            ok;
        {Res, Val} ->
            ?info_else_warning(Res=:=ok, 'tracing',
                                "Started tracer : ~w", [Res],
                                [{'tracer_type',TracerType},
                                 {'returned',{Res,Val}}])
    end.

apply_start_viewer(undefined, _) ->
    ok;
apply_start_viewer(false, _) ->
    ok;
apply_start_viewer(true, io) ->
    ?warning('replay', "et_viewer not compatible with io tracing.",[],
             {'tracer_type', io});
apply_start_viewer(true, TracerType) ->
    {Res, Val} = case TracerType of
                   {Path, _} when is_list(Path) ->
                        sa_debug:start_et_viewer(Path);
                    TCPSpecOrPath ->
                        sa_debug:start_et_viewer(TCPSpecOrPath)
                 end,
    ?info_else_warning(Res=:=ok, 'replay', "Started et_viewer : ~w", [Res],
                        [{'tracer_type',TracerType}, {'returned',{Res,Val}}]).
          

apply_trace_flags(TraceFlags0, TracerType) ->
    TraceFlags1 =   case TraceFlags0 of
                        undefined -> [sol, c]; % Just enough for sa_debug.hrl ?trace_me macros
                        L -> L
                    end,
    {PRes, PVal} = dbg:p(?MODULE, TraceFlags1),
    ?info_else_warning(PRes=:=ok, 'tracing',
                        "Top SmokeAlarm Supervisor tracing : ~w", [PRes],
                        [{'tracer_type',TracerType}, {trace_flags,TraceFlags1},
                         {'returned',{PRes,PVal}}]),
    TraceFlags1.

apply_trace_me(undefined, _) ->
    ok;
apply_trace_me(false, _) ->
    ok;
apply_trace_me(true, TracerType) ->
    {Res, Val} = smokealarm:enable_trace_me(true, TracerType),
    ?info_else_warning(Res=:=ok, 'tracing',
        "Tracing et:trace_me calls from all SmokeAlarm modules : ~w", [Res],
        [{'tracer_type',TracerType}, {'returned',{Res,Val}}]).

apply_trace_calls(undefined, _) ->
    ok;
apply_trace_calls(false, _) ->
    ok;
apply_trace_calls(true, TracerType) ->
    Val = sa_debug:enable_mods_trace(smokealarm:get_modules(), calls, true),
    Res = {What,Why} =
        case Val of
            [] ->
                {error, 'not_found'};
            _ -> 
                case lists:takewhile(fun
                                        ({_,{ok,_}}) -> true;
                                        (_) -> false
                                     end, Val)
                of
                    [] ->   {error, 'all'};
                    Val ->  {ok, 'all'};
                    _ ->    {error, 'some'}
                end
        end,
    ?info_else_warning(What=:=ok, 'tracing',
        "Tracing all global and local calls for all SmokeAlarm modules : ~w ~w",
        [Why,What], [{'tracer_type',TracerType}, {'returned',{Res,Val}}]).
