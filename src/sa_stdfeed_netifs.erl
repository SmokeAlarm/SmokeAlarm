%%%-----------------------------------------------------------------------------
%%%
%%% Desc:       Predefined SmokeAlarm network interface feed.
%%%
%%% Purpose:    Monitor OS network interfaces (optionally on multiple nodes)
%%%             and tell a SmokeAlarm detector about...
%%%             - Added/removed interfaces
%%%             - Updates to interface configurations (e.g. address, netmask,
%%%               etc) and/or information about interfaces (running, up/down,
%%%               etc).
%%%
%%%-----------------------------------------------------------------------------

-module(sa_stdfeed_netifs).
-legal("Copyright (c) 2011, The Authors.\n"
        "http://github.com/SmokeAlarm\n"
        "See LICENSE.txt for licensing information.").
-author("Edmond Begumisa <ebegumisa at hysteria dash tech dot com>").

%% Public Interface
-export([subscribe/2, unsubscribe/2]).

%% Private Interface
-export([local_init/2, remote_init/3]).

%% Private Preprocess
-define(DFLT_POLL_INTERVAL, {5,seconds}).
-record(opts, {poll_interval, relay, nodes}).
-record(relay, {initial_ifaces, initial_iface,
                new_ifaces, new_iface,
                updated_ifaces, updated_iface,
                removed_ifaces, removed_iface}).
-include("sa_utils.hrl").
-import(smokealarm, [report_event/2, report_event/3,
                     report_multi_simul_events/3]).

%%% Interface ------------------------------------------------------------------

subscribe(SubscriberId, Opts) when is_list(Opts) ->
    FeedP = spawn_link(?MODULE, local_init, [SubscriberId, to_opts_rec(Opts)]),
    {ok, FeedP}.
        
unsubscribe(_SubscriberId, FeedP) ->
    FeedP ! {stop, self()},
    ok. 

%%% Local Process Implementation -----------------------------------------------

local_init(SubrId, #opts{nodes=Ndes,relay=#relay{}=Relay}=Opts) ->
    put('subscriber_id', SubrId),
    process_flag(trap_exit, true),
    RemotePL =  case Ndes of
                    local ->
                        [];
                    _ ->
                        ok = net_kernel:monitor_nodes(true),
                        local_add_nodes(SubrId, Opts, case Ndes of 
                                                         [] -> nodes();
                                                         _ -> Ndes
                                                      end)
                end,
    {ok, InitIFaces} = inet:getifaddrs(),
    maybe_report_initial(SubrId, Relay, InitIFaces, {node(),self(),now()}),
    local_loop(SubrId, Opts, {RemotePL,InitIFaces}).

local_add_nodes(SubrId, Opts, Ndes) when is_list(Ndes) ->
    [local_add_nodes(SubrId, Opts, Nde) || Nde <- Ndes];
local_add_nodes(SubrId, Opts, Nde) ->
    {?MODULE, Bin, FileName} = code:get_object_code(?MODULE),
    case rpc:call(Nde, code, load_binary, [?MODULE, FileName, Bin]) of
        {badrpc, nodedown} ->
            ok;
        {'module', ?MODULE} ->
            _ = spawn_link(Nde, ?MODULE, remote_init, [SubrId, Opts, self()]),
            ok
    end.

local_loop(SubrId, #opts{poll_interval=PollT,relay=#relay{}=Relay}=Opts,
           {RemotePL0,PrevIFaces}=S0)
    ->
    receive
        {remote_initial, InitIFaces, Origin} ->
            ok = maybe_report_initial(SubrId, Relay, InitIFaces, Origin),
            local_loop(SubrId, Opts, S0);
        {remote_changes, Changes, Origin} ->
            ok = maybe_report_changes(SubrId, Relay, Changes, Origin),
            local_loop(SubrId, Opts, S0);
        {nodeup, Nde} ->
            case Opts#opts.nodes=:=[] 
                 orelse lists:member(Nde, Opts#opts.nodes)
            of
                true ->
                    RemotePL1 = [local_add_nodes(SubrId, Opts, Nde) 
                                    | RemotePL0],
                    local_loop(SubrId, Opts, {RemotePL1,PrevIFaces});
                false ->
                    local_loop(SubrId, Opts, S0)
            end;
        {nodedown, _} ->
            local_loop(SubrId, Opts, S0);
        {'EXIT', noconnection, _} ->
            local_loop(SubrId, Opts, S0);
        {stop, _} ->
            lists:foreach(
                fun(RemoteP) -> RemoteP ! {stop,self()} end, RemotePL0), 
            ok
    after PollT ->
        {ok, CurrIFaces} = inet:getifaddrs(),
        case get_changes(CurrIFaces, PrevIFaces, Relay) of
            {[],[],[]} ->
                ok;
            Changes ->
                Origin = {node(),self(),now()},
                ok = maybe_report_changes(SubrId, Relay, Changes, Origin)
        end,
        local_loop(SubrId, Opts, {RemotePL0,CurrIFaces})
    end.
    
%%% Remote Process Implementation ----------------------------------------------

remote_init(SubrId, #opts{}=Opts, LocalP) ->    
    put('subscriber_id', SubrId),
    {ok, InitIFaces} = inet:getifaddrs(),
    LocalP ! {remote_initial, InitIFaces, {node(),self(),now()}},
    remote_loop(SubrId, Opts, InitIFaces, LocalP).
    
remote_loop(SubrId, #opts{poll_interval=PollT,relay=#relay{}=Relay}=Opts,
            PrevIFaces, LocalP)
    ->
    receive
        {stop, LocalP} -> ok
    after PollT ->
        {ok, CurrIFaces} = inet:getifaddrs(),
        case get_changes(CurrIFaces, PrevIFaces, Relay) of
            {[],[],[]} ->
                ok;
            Changes ->
                LocalP ! {remote_changes, Changes, {node(),self(),now()}}
        end,
        remote_loop(SubrId, Opts, CurrIFaces, LocalP)
    end.

%%% Helpers --------------------------------------------------------------------

get_changes(CurrIFaces, PrevIFaces, Relay) ->
    {UpdIFaces,NewIFaces,RemIFaces} =
        lists:foldl(
            fun ({K,CurrProps}=CurrIFace, {UpdAcc0,NewAcc0,PrevDec0}) ->
                case lists:keytake(K, 1, PrevDec0) of
                    {value, CurrIFace, PrevDec1} ->
                        {UpdAcc0, NewAcc0, PrevDec1};
                    {value, {K,PrevProps}, PrevDec1} when
                    Relay#relay.updated_ifaces; Relay#relay.updated_iface
                    ->
                        {[{K,PrevProps,CurrProps}|UpdAcc0], NewAcc0, PrevDec1};
                    {value, {K,_}, PrevDec1} ->
                        {UpdAcc0,NewAcc0,PrevDec1};
                    false when Relay#relay.new_ifaces; Relay#relay.new_iface ->
                        {UpdAcc0, [CurrIFace|NewAcc0], PrevDec0};
                    false ->
                        {UpdAcc0, NewAcc0, PrevDec0}
                end
            end, {[],[],PrevIFaces}, CurrIFaces),
    {lists:reverse(UpdIFaces), lists:reverse(NewIFaces), RemIFaces}.

maybe_report_initial(SubrId, Relay, InitIFaces, Origin) ->
    if
        Relay#relay.initial_ifaces ->
            ok = report_event(SubrId, {initial_ifaces, InitIFaces}, Origin);
        Relay#relay.initial_iface ->
            InitEvents = [{initial_iface, InitIFace}
                            || InitIFace <- InitIFaces],
            ok = report_multi_simul_events(SubrId, InitEvents, Origin);
        true ->
            ok
    end.

maybe_report_changes(SubrId, Relay, {UpdIFaces,NewIFaces,RemIFaces}, Origin) ->
    case UpdIFaces of
        [] ->
            ok;
        _ when Relay#relay.updated_ifaces ->
            ok = report_event(SubrId, {updated_ifaces, UpdIFaces}, Origin);
        _ when Relay#relay.updated_iface ->
            UpdEvents = [{updated_iface, UpdIFace} || UpdIFace <- UpdIFaces],
            ok = report_multi_simul_events(SubrId, UpdEvents, Origin);
        _ ->
            ok
    end,
    case NewIFaces of
        [] ->
            ok;
        _ when Relay#relay.new_ifaces ->
            ok = report_event(SubrId, {new_ifaces, NewIFaces}, Origin);
        _ when Relay#relay.new_iface ->
            NewEvents = [{new_iface, NewIFace} || NewIFace <- NewIFaces],
            ok = report_multi_simul_events(SubrId, NewEvents, Origin);
        _ ->
            ok
    end,
    case RemIFaces of
        [] ->
            ok;
        _ when Relay#relay.removed_ifaces ->
            ok = report_event(SubrId, {removed_ifaces, RemIFaces}, Origin);
        _ when Relay#relay.removed_iface ->
            RemEvents = [{removed_iface, RemIFace} || RemIFace <- RemIFaces],
            ok = report_multi_simul_events(SubrId, RemEvents, Origin);
        _ ->
            ok
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

chk_opt({nodes, undefined}) ->
    {nodes, []};
chk_opt({nodes, local}=Good) ->
    Good;
chk_opt({nodes, V}=Good) when is_list(V) ->
    Good;
chk_opt({poll_interval, undefined}) ->
    chk_opt({poll_interval, ?DFLT_POLL_INTERVAL});
chk_opt({poll_interval, N}=Good) when is_number(N) ->
    Good;
chk_opt({poll_interval, {N,milliseconds}}=Good) when is_number(N) ->
    Good;
chk_opt({poll_interval, {N,TUnit}}=Opt) when is_number(N), is_atom(TUnit) ->
    try {poll_interval, timer:TUnit(N)}
    catch _:Why -> throw({'bad_opt', {Opt,Why}})
    end;    
chk_opt({relay, undefined}) ->
    {relay, to_relay_rec([])};
chk_opt({relay, V0}) when is_list(V0) ->
    {relay, to_relay_rec(V0)};
chk_opt(Bad) ->
    throw({'bad_opt', Bad}).

to_relay_rec([]) ->
    to_relay_rec([initial_ifaces, new_ifaces, updated_ifaces]);
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
