%%%-----------------------------------------------------------------------------
%%%
%%% Desc:       Predefined SmokeAlarm nodes feed.
%%%
%%% Purpose:    Tell a smokealarm detector about connected/disconnected nodes.
%%%
%%%-----------------------------------------------------------------------------

-module(sa_stdfeed_nodes).
-legal("Copyright (c) 2011, The Authors.\n"
        "http://github.com/SmokeAlarm\n"
        "See LICENSE.txt for licensing information.").
-author("Edmond Begumisa <ebegumisa at hysteria dash tech dot com>").

%% Public Interface
-export([subscribe/2, unsubscribe/2]).

%% Private Interface
-export([init/2]).

%% Private Preprocess
-record(opts, {relay, nodes}).
-record(relay, {initial_nodes, initial_node,
                nodeup, nodedown}).
-include("sa_utils.hrl").
-import(smokealarm, [report_event/2, report_event/3,
                     report_multi_simul_events/3]).

%%% Interface ------------------------------------------------------------------

subscribe(SubscriberId, Opts) when is_list(Opts) ->
    FeedP = spawn_link(?MODULE, init, [SubscriberId, to_opts_rec(Opts)]),
    {ok, FeedP}.
        
unsubscribe(_SubscriberId, FeedP) ->
    FeedP ! {stop, self()},
    ok. 

%%% Process Implementation -----------------------------------------------------

init(SubrId, #opts{nodes=Ndes,relay=#relay{}=Relay}=Opts) ->
    put('subscriber_id', SubrId),
    process_flag(trap_exit, true),
    ok = net_kernel:monitor_nodes(true),
    ok = maybe_report_initial(SubrId, Relay, Ndes),
    loop(SubrId, Opts).

loop(SubrId, #opts{relay=#relay{}=Relay}=Opts) ->
    case 
        receive
            {nodeup, _}=Evt when Relay#relay.nodeup ->
                ok = maybe_report_node(SubrId, Evt, Opts#opts.nodes);
            {nodeup, _} ->
                ok;
            {nodedown, _}=Evt when Relay#relay.nodedown ->
                ok = maybe_report_node(SubrId, Evt, Opts#opts.nodes);
            {nodedown, _} ->
                ok;
            {stop, _} ->
                stop
        end
    of
        ok -> loop(SubrId, Opts);
        stop -> ok
    end.

%%% Helpers  -------------------------------------------------------------------

maybe_report_initial(SubrId, Relay, Ndes0) ->
    Ndes1 = case Ndes0 of 
                [] -> nodes();
                _ -> Ndes0
            end,
    Origin = {node(),self(),now()},
    if
        Relay#relay.initial_nodes ->
            ok = report_event(SubrId, {initial_nodes, Ndes1}, Origin);
        Relay#relay.initial_node ->
            InitEvents = [{initial_node, Nde} || Nde <- Ndes1],
            ok = report_multi_simul_events(SubrId, InitEvents, Origin);
        true ->
            ok
    end.

maybe_report_node(SubrId, {_,Nde}=Evt, Ndes) ->
    case Ndes=:=[] orelse lists:member(Nde, Ndes) of
        true -> ok = report_event(SubrId, Evt, {node(),self(),now()});
        false -> ok
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
chk_opt({nodes, V}=Good) when is_list(V)->
    Good;
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

chk_relay_item({EvtTag, undefined}) ->
    {EvtTag, false};
chk_relay_item({_, V}=Good) when is_boolean(V) ->
    Good;
chk_relay_item(Bad) ->
    throw({'bad_event', Bad}).
