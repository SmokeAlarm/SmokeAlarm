%%%-----------------------------------------------------------------------------
%%%
%%% Desc:       Predefined Detector Specifications for Networks
%%%
%%% Purpose:    Provide detection specifications for dead networks, lost nodes,
%%%             node partitions, etc. 
%%%
%%%-----------------------------------------------------------------------------

-module(sa_stddetectors_net).
-legal("Copyright (c) 2011, The Authors.\n"
        "http://github.com/SmokeAlarm\n"
        "See LICENSE.txt for licensing information.").
-author("Edmond Begumisa <ebegumisa at hysteria dash tech dot com>").

%% Public Interface
-export([network_down_specs/0]).
-export([node_lost_specs/0]).

%% Private Interface
-export([]).

%% Private Preprocess 
-define(DFLT_REACTORS, [{sa_stdrtor_errlog, {sa_stdreactor_errlog,[]}}]).
    
%%% Interface ------------------------------------------------------------------

network_down_specs() ->
    network_down_specs([]).
    
network_down_specs(SpecsIn) when is_list(SpecsIn) ->
    NetIFsFeed = {sa_stdfeed_netifs,
                    [{relay, [removed_iface,updated_iface,new_iface]}]},
    DfltFeeds = [{stdfeed_netifs, NetIFsFeed}],
    do_network_down_specs(
        proplists:get_value(feeds, SpecsIn, DfltFeeds),
        proplists:get_value(reactors, SpecsIn, ?DFLT_REACTORS)).

do_network_down_specs(Feeds, Reactors) ->    
    TripTrigger =
        [{match,
            fun
                ({removed_iface,{K,Props}=IFace}, {Nde,_,TS}, DownAcc) ->
                    case iface_is_active(Props) of
                        false -> false;
                        true->
                            Res = [{'iface_deactivated', IFace},
                                   {'node', Nde},
                                   {'timestamp', TS}],
                            {trigger, [{Nde,K}|DownAcc], Res}
                    end;
                ({updated_iface,{K,OldProps,NewProps}}, {Nde,_,TS}, DownAcc) ->
                    case iface_is_up_to_down(OldProps, NewProps) of
                        false -> false;
                        true ->
                            Res = [{'iface_down', {K,NewProps}},
                                   {'node', Nde},
                                   {'timestamp', TS}],
                            {trigger, [{Nde,K}|DownAcc], Res}
                    end;
                (_,_,_) ->
                    false
            end}],
    ResetTrigger =
        [{match,
            fun
                ({added_iface,{K,Props}=IFace}, {Nde,_,TS}, DownAcc) ->
                    case lists:member({Nde,K}, DownAcc) andalso
                         iface_is_active(Props)
                    of
                        false -> false;
                        true->
                            Res = [{'iface_reactivated', IFace},
                                   {'node', Nde},
                                   {'timestamp', TS}],
                            {trigger, lists:delete({Nde,K}, DownAcc), Res}
                    end;
                ({updated_iface,{K,OldProps,NewProps}}, {Nde,_,TS}, DownAcc) ->
                    case lists:member({Nde,K}, DownAcc) andalso
                         iface_is_down_to_up(OldProps, NewProps)
                    of
                        false -> false;
                        true ->
                            Res = [{'iface_back_up', {K,NewProps}},
                                   {'node', Nde},
                                   {'timestamp', TS}],
                            {trigger, lists:delete({Nde,K}, DownAcc), Res}
                    end;
                (_,_,_) ->
                    false
            end}],
    [{feeds, Feeds},
     {triggers, {TripTrigger,ResetTrigger,[]}},
     {reactors, Reactors}].

iface_is_active(Props) ->
    proplists:is_defined(addr, Props) 
        andalso proplists:is_defined(up, Flags =
                                          proplists:get_value(flags, Props,[]))
        andalso proplists:is_defined(running, Flags).

iface_is_up_to_down(PropsA, PropsB) ->
    case proplists:is_defined(addr, PropsA) of
        false -> false;
        true ->
            FlagsA = proplists:get_value(flags, PropsA, []),
            FlagsB = proplists:get_value(flags, PropsB, []),
            (proplists:is_defined(up, FlagsA) 
              andalso not proplists:is_defined(up, FlagsB))
            orelse
            (proplists:is_defined(running, FlagsA) 
              andalso not proplists:is_defined(running, FlagsB))
    end.

iface_is_down_to_up(PropsA, PropsB) ->
    iface_is_up_to_down(PropsB, PropsA).


node_lost_specs() ->
    node_lost_specs([]).
    
node_lost_specs(SpecsIn) when is_list(SpecsIn) ->
    NdesFeed = {sa_stdfeed_nodes, [{relay, [nodedown,nodeup]}]},
    DfltFeeds = [{stdfeed_nodes, NdesFeed}],
    do_node_lost_specs(
        proplists:get_value(feeds, SpecsIn, DfltFeeds),
        proplists:get_value(reactors, SpecsIn, ?DFLT_REACTORS)).

do_node_lost_specs(Feeds, Reactors) ->    
    TripTrigger =
        [{match,
            fun
                ({nodedown,TargNde,Info}, {SrcNde,_,TS}, LostNdes) ->
                    case proplists:get_value(nodedown_reason,Info) of
                        undefined -> false;
                        disconnect -> false;
                        _ ->
                            Res = {SrcNde, TS, {node_lost,TargNde}},
                            {true, [{SrcNde,TargNde}|LostNdes], Res}
                    end;
                (_,_,_) ->
                    false
            end}],
    ResetTrigger =
        [{match,
            fun
                ({nodeup,TargNde}, {SrcNde,_,TS}, LostAcc) ->
                    case lists:member(TargNde, LostAcc) of
                        false -> false;
                        true->
                            Res = {SrcNde, TS, {node_refound, TargNde}},
                            {true, lists:delete(TargNde, LostAcc), Res}
                    end;
                (_,_,_) ->
                    false
            end}],
    [{feeds, Feeds},
     {triggers, {TripTrigger,ResetTrigger,[]}},
     {reactors, Reactors}].
    
    