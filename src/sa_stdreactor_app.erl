%%%-----------------------------------------------------------------------------
%%%
%%% Desc:       Predefined SmokeAlarm reactor for OTP Applications
%%%             
%%% Purpose:    Avail a wide range of OTP application related actions for
%%%             reacting to a SmokeAlarm detector trip and/or reset ranging
%%%             from application restart to setting/unsetting env values and
%%%             more.
%%%
%%%-----------------------------------------------------------------------------

-module(sa_stdreactor_app).
-legal("Copyright (c) 2011, The Authors.\n"
        "http://github.com/SmokeAlarm\n"
        "See LICENSE.txt for licensing information.").
-author("Edmond Begumisa <ebegumisa at hysteria dash tech dot com>").

%% Public Interface
-export([handle_tripped/4, handle_reset/3]).
                
%%% Interface ------------------------------------------------------------------

handle_tripped(_ReactorId, TrippedResult, [ActionOrL], _PipeAcc) ->
    react(ActionOrL, TrippedResult).
    
handle_reset(_ReactorId, ResetResult, [ActionOrL]) ->
    react(ActionOrL, ResetResult).

react(Actions, Targets) when is_list(Actions) ->
    foreach_while_ok(fun (Action) -> action(Action, Targets) end, Actions);
react(Action, Targets) ->
    action(Action, Targets).
    
action(Action, Targets) when is_list(Targets) ->
    foreach_while_ok(fun (Target) -> do_action(Action, Target) end, Targets);
action(_, Invalid) ->
    throw({bad_action,Invalid}).
        
do_action(load, {AppDescr, Distributed}) ->
    case application:load(AppDescr, Distributed) of
        ok -> ok;
        {error, {already_loaded,AppDescr}} -> ok;
        Err -> Err
    end;
do_action(load, AppDescr) ->
    case application:load(AppDescr) of
        ok -> ok;
        {error, {already_loaded,AppDescr}} -> ok;
        Err -> Err
    end;
do_action(unload, App) ->
    case application:unload(App) of
        ok -> ok;
        {error, {not_loaded,App}} -> ok;
        Err -> Err
    end;
do_action(ensure_unloaded, App) ->
    case application:unload(App) of
        ok -> ok;
        {error, {not_loaded,App}} -> ok;
        {error, {running,App}} ->
            case do_action(stop, App) of
                ok -> do_action(unload, App);
                Err -> Err
            end;
        Err -> Err
    end;
do_action(reload, {application,App,_}=AppSpec) ->
    case do_action(unload, App) of
        ok -> do_action(load, AppSpec);
        Err -> Err
    end;
do_action(reload, {{application,App,_}, _}=AppDescr) ->
    case do_action(unload, App) of
        ok -> do_action(load, AppDescr);
        Err -> Err
    end;
do_action(reload, {App,_}=AppDescr) ->
    case do_action(unload, App) of
        ok -> do_action(load, AppDescr);
        Err -> Err
    end;
do_action(reload, App) ->
    case do_action(unload, App) of
        ok -> do_action(load, App);
        Err -> Err
    end;
do_action(ensure_reloaded, {application,App,_}=AppSpec) ->
    case do_action(ensure_unloaded, App) of
        ok -> do_action(load, AppSpec);
        Err -> Err
    end;
do_action(ensure_reloaded, {{application,App,_}, _}=AppDescr) ->
    case do_action(ensure_unloaded, App) of
        ok -> do_action(load, AppDescr);
        Err -> Err
    end;
do_action(ensure_reloaded, {App,_}=AppDescr) ->
    case do_action(ensure_unloaded, App) of
        ok -> do_action(load, AppDescr);
        Err -> Err
    end;
do_action(ensure_reloaded, App) ->
    case do_action(ensure_unloaded, App) of
        ok -> do_action(load, App);
        Err -> Err
    end;
do_action(start, {App,Type}) ->
    case application:start(App, Type) of
        ok -> ok;
        {error, {already_started,App}} -> ok;
        Err -> Err
    end;
do_action(start, App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started,App}} -> ok;
        Err -> Err
    end;
do_action(stop, App) ->
    case application:stop(App) of
        ok -> ok;
        {error, {not_started,App}} -> ok;
        Err -> Err
    end;
do_action(restart, {App,_}=Args) ->
    case do_action(stop, App) of
        ok -> do_action(start, Args);
        Err -> Err
    end;
do_action(restart, App) ->
    case do_action(stop, App) of
        ok -> do_action(start, App);
        Err -> Err
    end;
do_action(reload_restart, {App,_}=Args) ->
    case do_action(ensure_reloaded, App) of
        ok -> do_action(start, Args);
        Err -> Err
    end;
do_action(reload_restart, App) ->
    case do_action(ensure_reloaded, App) of
        ok -> do_action(start, App);
        Err -> Err
    end;
do_action(permit, {App,Val}) ->
    application:permit(App, Val);
do_action(permit, Invalid) ->
    throw({bad_action,Invalid});
do_action(ensure_permit, {App,Val}) ->
    case application:permit(App, Val) of
        ok -> ok;
        {error, {not_loaded,App}} ->
            case do_action(load, App) of
                ok -> application:permit(App, Val);
                Err -> Err
            end;
        Err -> Err
    end;
do_action(ensure_permit, Invalid) ->
    throw({bad_action,Invalid});
do_action(takeover, {App,Type}) ->
    application:takeover(App, Type);
do_action(takeover, Invalid) ->
    throw({bad_action,Invalid});
do_action(set_env, {App,Par,Val}) ->
    application:set_env(App, Par, Val);
do_action(set_env, {App,Par,Val,Timeout}) ->
    application:set_env(App, Par, Val, Timeout);
do_action(set_env, Invalid) ->
    throw({bad_action,Invalid});
do_action(set_env_when_undefined, {App,Par,Val}) ->
    case application:get_env(App, Par) of
        {ok, undefined} -> application:set_env(App, Par, Val);
        _ -> ok
    end;
do_action(set_env_when_undefined, {App,Par,Val,Timeout}) ->
    case application:get_env(App, Par) of
        {ok, undefined} -> application:set_env(App, Par, Val, Timeout);
        _ -> ok
    end;
do_action(set_env_when_undefined, Invalid) ->
    throw({bad_action,Invalid});
do_action(set_env_when_defined, {App,Par,Val}) ->
    case application:get_env(App, Par) of
        {ok, undefined} -> ok;
        _  -> application:set_env(App, Par, Val)
    end;
do_action(set_env_when_defined, {App,Par,Val,Timeout}) ->
    case application:get_env(App, Par) of
        {ok, undefined} -> ok;
        _ -> application:set_env(App, Par, Val, Timeout)
    end;
do_action(set_env_when_defined, Invalid) ->
    throw({bad_action,Invalid});
do_action(unset_env, {App,Par}) ->
    application:unset_env(App, Par);
do_action(unset_env, {App,Par,Timeout}) ->
    application:unset_env(App, Par, Timeout);
do_action(unset_env, Invalid) ->
    throw({bad_action,Invalid});
do_action(unset_env_when_undefined, {App,Par}) ->
    case application:get_env(App, Par) of
        {ok, undefined} -> application:unset_env(App, Par);
        _ -> ok
    end;
do_action(unset_env_when_undefined, {App,Par,Timeout}) ->
    case application:get_env(App, Par) of
        {ok, undefined} -> application:unset_env(App, Par, Timeout);
        _ -> ok
    end;
do_action(unset_env_when_undefined, Invalid) ->
    throw({bad_action,Invalid});
do_action(unset_env_when_defined, {App,Par}) ->
    case application:get_env(App, Par) of
        {ok, undefined} -> ok;
        _  -> application:unset_env(App, Par)
    end;
do_action(unset_env_when_defined, {App,Par,Timeout}) ->
    case application:get_env(App, Par) of
        {ok, undefined} -> ok;
        _ -> application:unset_env(App, Par, Timeout)
    end;
do_action(unset_env_when_defined, Invalid) ->
    throw({bad_action,Invalid});
do_action(Invalid, _) ->
    throw({unknown_action,Invalid}).

%%% Helpers --------------------------------------------------------------------

foreach_while_ok(F, L) ->
    sa_utils:lists_foldl_while(
                fun (X, ok) ->
                    case F(X) of
                        ok  -> {true, ok};
                        Err -> {false, Err}
                    end
                end, ok, L).
