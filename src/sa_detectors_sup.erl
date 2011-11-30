%%%-----------------------------------------------------------------------------
%%%
%%% Desc:       Top-level (Dynamic) Supervisor Process
%%%
%%% Purpose:    Add/remove detectors from/to SmokeAlarm, restart them if they
%%%             crash.
%%%
%%%-----------------------------------------------------------------------------

-module(sa_detectors_sup).
-legal("Copyright (c) 2011, The Authors.\n"
        "http://github.com/SmokeAlarm\n"
        "See LICENSE.txt for licensing information.").
-author("Edmond Begumisa <ebegumisa at hysteria dash tech dot com>").

%% Public Control
-export([dev_start_stand_alone/1, dev_start_stand_alone/0,
         dev_stop_stand_alone/0]).

%% Public Interface
-export([start_child/4]).

-export([dev_find/1]).

%% Private Control
-export([start_link/1]).

%% Private Interface
-behaviour(supervisor).
-export([init/1]).

%% Private Preprocess
-define(TRACE_ME_DETAIL, 10). % Above top sup, below detectors.
-include("common.hrl").

%%% Control --------------------------------------------------------------------

start_link(AppConf) ->
    supervisor:start_link({local,?MODULE}, ?MODULE, AppConf).


%% Start/stop directly without the application controller.
%% These are intended for *internal* SA testing and development only.

dev_start_stand_alone() ->
    dev_start_stand_alone(#app_conf{}).
    
dev_start_stand_alone(AppConf) when is_list(AppConf) ->
    dev_start_stand_alone(?PROPLIST_TO_RECORD(AppConf, app_conf));    
dev_start_stand_alone(#app_conf{}=AppConf) ->
    Ret = {ok, Pid} = start_link(AppConf),
    unlink(Pid),
    Par = list_to_atom(?MODULE_STRING "_stand_alone_pid"),
    ok = application:set_env(kernel, Par, Pid),
    Ret.
    
dev_stop_stand_alone() ->
    Par = list_to_atom(?MODULE_STRING "_stand_alone_pid"),
    {ok, Pid} = application:get_env(kernel, Par),
    true = exit(Pid, normal),
    ok = application:unset_env(kernel, Par).

%% Get a detector Id from a detector key.
%% This is VERY inefficient - never use in code, only in the shell for
%% troubleshooting.
dev_find(Key) ->
    case lists:dropwhile(
            fun({_,ChildP,_,_}) ->
                {dictionary, Props} = process_info(ChildP, dictionary),
                case proplists:get_value(detector_key, Props) of
                    Key -> false;
                    _ -> true
                end
            end, supervisor:which_children(?MODULE))
    of
        [] ->
            undefined;
        [{_,DetP,_,_}|_] ->
            {Key, DetP}
    end.
    
%%% Interface ------------------------------------------------------------------

%% This should not be called directly. Use smokealarm:start_detector instead.
start_child(Key, ESPs, InitFeeds, InitReactors) ->
    ReplayHandle =  case application:get_env(?APP, replay_handle) of
                        {ok, V} -> V;
                        _ -> undefined
                    end,
    supervisor:start_child(
        ?MODULE, [Key, ESPs, InitFeeds, InitReactors, ReplayHandle]).

%%% 'supervisor' callbacks -----------------------------------------------------

init(#app_conf{}=AppConf) ->
    ?trace_me(sa, sa, "Ready", self()), % Well, not exactly but almost!
    {ok, {{simple_one_for_one,
           AppConf#app_conf.child_restart_freq_maxr,
           AppConf#app_conf.child_restart_freq_maxt},
         [{{sa_detector,undefined},
           {sa_detector, start_link,
               [AppConf#app_conf.child_otp_start_opts,
                {AppConf#app_conf.dtor_faulty_freq_maxr,
                 AppConf#app_conf.dtor_faulty_freq_maxtu}]},
           transient, 
           AppConf#app_conf.child_shutdown_timeout,
           worker,
           [sa_detector]}]}}.
