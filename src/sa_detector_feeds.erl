%%%-----------------------------------------------------------------------------
%%%
%%% Desc:       KV Store of subscribed feeds
%%%
%%% Purpose:    - Handle requests sent to a detector to manage it's dictionary
%%%               of feeds.
%%%             - Reduce clutter in sa_detector.erl and make detectors easier to
%%%               trace by separating out feed management code.
%%%             
%%% Note:       This module, like it's owner (sa_detector.erl) is defensively
%%%             programmed because detectors are "kernels" that shouldn't be
%%%             crashed by feed management requests. Instead, faulty feeds
%%%             and bad requests are rejected here (possibly crashing the
%%%             requestor).
%%%
%%%-----------------------------------------------------------------------------

-module(sa_detector_feeds).
-legal("Copyright (c) 2011, The Authors.\n"
        "http://github.com/SmokeAlarm\n"
        "See LICENSE.txt for licensing information.").
-author("Edmond Begumisa <ebegumisa at hysteria dash tech dot com>").

%% Public Interface
-export([new/0]).
-export([add_feed/4, add_feeds/4,
         remove_feed/4, remove_feeds/4, remove_all_feeds/3,
         which_feeds/3]).

%% Private Interface
-export([]).

%% Private Preprocess
-define(TRACE_ME_DETAIL, 30). % Above detector, same as reactors.
-include("common.hrl").

%%% Interface ------------------------------------------------------------------

new() ->
    dict:new().

%%% NB: add_* functions all have side effects (calling subscribe).
    
add_feed({K,Spec}, ReqFrm, DetK, Feeds0) ->
    ?trace_me(ReqFrm, DetK, ?FUNCTION, {K,Spec}),
    Ret = {{What,_}=Reply, _} =
        case (dict:is_key(K, Feeds0)
              orelse
                try make_feed_funs(Spec)
                catch _:Why ->
                    Who = [{culprit, {K,Spec}},
                           {stacktrace, erlang:get_stacktrace()}],
                    {error, {'subscribe_feed_spec',Why,Who}}
                end)
        of
            true ->
                {{error,{already_exists,K}}, Feeds0};
            {error,_}=Err ->
                {Err, Feeds0};
            {SubF, SubArgs, UnsubF} ->
                SubscriberId = {DetK,self(),K},
                SubFInfo = erlang:fun_info(SubF),
                UnsubFInfo = erlang:fun_info(UnsubF),
                case
                    try SubF(SubscriberId, SubArgs)
                    catch _:Why1 ->
                        Who1 = [{culprit,
                                    {SubscriberId,{SubF,SubFInfo,SubArgs}}},
                                {stacktrace, erlang:get_stacktrace()}],
                        {error, {'subscribe_feed_fun',Why1,Who1}}
                    end
                of
                    {ok, UnsubArgs} ->
                        Feed = {UnsubF,UnsubFInfo,UnsubArgs},  
                        {{ok,{K,Feed}}, dict:store(K, Feed, Feeds0)};
                    {error,{'subscribe_feed_fun',_,_}}=Err ->
                        {Err, Feeds0};
                    Why2 ->
                        Who2 = {culprit,{SubscriberId,{SubF,SubFInfo,SubArgs}}},
                        Err = {error, {'subscribe_feed_return',Why2,Who2}},
                        {Err, Feeds0}
                end
        end,
    ?trace_me(DetK, ReqFrm, {What,?FUNCTION}, Reply),
    Ret.
    
add_feeds(KSpecPairs, ReqFrm, DetK, Feeds0) when is_list(KSpecPairs) ->
    ?trace_me(ReqFrm, DetK, ?FUNCTION, KSpecPairs),
    {OkL,ErrL,LastFeeds} =
        lists:foldl(
            fun
                ({K,Spec}, {OkAcc,ErrAcc,Feeds1}) ->
                    case add_feed({K,Spec}, DetK, DetK, Feeds1) of
                        {{ok,_}=Ok, D2} ->
                            {[Ok|OkAcc], ErrAcc, D2};
                        {{error,_}=Err, D2} ->
                            {OkAcc, [Err|ErrAcc], D2}
                    end;
                (Invalid, {OkAcc,ErrAcc,Feeds1}) ->
                    Err = {error, {?FUNCTION,"Expecting a tuple 2.",Invalid}},
                    {OkAcc, [Err|ErrAcc], Feeds1}
            end, {[],[],Feeds0}, KSpecPairs),
    Reply = {What,_} = 
        case ErrL of
            []  ->
                {ok, lists:reverse(OkL)};
            _ ->
                {error, {?FUNCTION,"Failed to subscribe to one or more feeds.",
                           [{failures,lists:reverse(ErrL)},
                            {successes,lists:reverse(OkL)}]}}
        end,
    ?trace_me(DetK, ReqFrm, {What,?FUNCTION}, Reply),
    {Reply, LastFeeds};
add_feeds(Invalid, ReqFrm, DetK, Feeds) ->
    ?trace_me(ReqFrm, DetK, ?FUNCTION, Invalid),
    Reply = {error, {?FUNCTION,"Expecting a tuple property list.",Invalid}},
    ?trace_me(DetK, ReqFrm, {error,?FUNCTION}, Reply),
    {Reply, Feeds}.

%%% NB: remove_* functions all have side effects (calling unsubscribe).

remove_feed(K, ReqFrm, DetK, Feeds0) ->
    ?trace_me(ReqFrm, DetK, ?FUNCTION, K),
    Ret = {{What,_}=Reply, _} = 
        case dict:find(K, Feeds0) of
            error ->
                {{error,{not_found,K}}, Feeds0};
            {ok, {UnsubF,_UnsubFInfo,UnsubArgs}=Feed}  ->
                SubsciberId={DetK,self(),K},
                case
                    try UnsubF(SubsciberId, UnsubArgs)
                    catch _:Why ->
                        Who = [{culprit, {SubsciberId,Feed}},
                               {stacktrace, erlang:get_stacktrace()}],
                        {error, {'unsubscribe_feed_fun',Why,Who}}
                    end
                of
                    ok ->
                        {{ok,{K,Feed}}, dict:erase(K, Feeds0)};
                    {error,{'unsubscribe_feed_fun',_,_}} = Err ->
                        {Err, Feeds0};
                    Why1 ->
                        Who1 = {culprit,{SubsciberId,Feed}},
                        {{error, {'unsubscribe_feed_return',Why1,Who1}}, Feeds0}
                end
        end,
    ?trace_me(DetK, ReqFrm, {What,?FUNCTION}, Reply),
    Ret.
    
remove_feeds(KL, ReqFrm, DetK, Feeds0) when is_list(KL) ->
    ?trace_me(ReqFrm, DetK, ?FUNCTION, KL),
    {OkL,ErrL,LastFeeds} =
        lists:foldl(
            fun (K, {OkAcc,ErrAcc,Feeds1}) ->
                case remove_feed(K, DetK, DetK, Feeds1) of
                    {{ok,_}=Ok, D2} ->
                        {[Ok|OkAcc], ErrAcc, D2};
                    {{error,_}=Err, D2} ->
                        {OkAcc, [Err|ErrAcc], D2}
                end
            end, {[],[],Feeds0}, KL),
    Reply = {What,_} = 
        case ErrL of
            []  -> {ok, lists:reverse(OkL)};
            _   -> {error, {?FUNCTION,
                            "Failed to unsubscribe from one or more feeds.",
                            [{failures,lists:reverse(ErrL)},
                             {successes,lists:reverse(OkL)}]}}
        end,
    ?trace_me(DetK, ReqFrm, {What,?FUNCTION}, Reply),
    {Reply, LastFeeds};
remove_feeds(Invalid, ReqFrm, DetK, Feeds) ->
    ?trace_me(ReqFrm, DetK, ?FUNCTION, Invalid),
    Reply = {error, {?FUNCTION,"Expecting a list.",Invalid}},
    ?trace_me(DetK, ReqFrm, {error,?FUNCTION}, Reply),
    {Reply, Feeds}.
    
remove_all_feeds(ReqFrm, DetK, Feeds0) ->
    ?trace_me(ReqFrm, DetK, ?FUNCTION),
    Ret = {{What,_}=Reply, _} =
        remove_feeds(dict:fetch_keys(Feeds0), DetK, DetK, Feeds0),
    ?trace_me(DetK, ReqFrm, {What,?FUNCTION}, Reply),
    Ret.

which_feeds(ReqFrm, DetK, Feeds) ->
    ?trace_me(ReqFrm, DetK, ?FUNCTION),
    Reply = {ok, dict:to_list(Feeds)},
    ?trace_me(DetK, ReqFrm, {ok,?FUNCTION}, Reply),
    {Reply, Feeds}.

%%% Helpers --------------------------------------------------------------------

make_feed_funs({M,SubArgs}) ->
    SubF = erlang:make_fun(M, subscribe, 2),
    UnsubF = erlang:make_fun(M, unsubscribe, 2),
    {SubF, SubArgs, UnsubF};
make_feed_funs([{SubM,SubFAtom,SubArgs}, UnsubSpec])  ->
    SubF = erlang:make_fun(SubM, SubFAtom, 2),
    {SubF, SubArgs, make_unsubscribe_fun(UnsubSpec)};
make_feed_funs([{SubF,SubArgs}, UnsubSpec]) when is_function(SubF, 2) ->
    {SubF, SubArgs, make_unsubscribe_fun(UnsubSpec)};
make_feed_funs(Invalid) ->
    throw({badarg,Invalid}).

make_unsubscribe_fun({UnsubM,StopFAtom}) ->
    erlang:make_fun(UnsubM, StopFAtom, 2);
make_unsubscribe_fun(UnsubF) when is_function(UnsubF, 2) ->
    UnsubF;
make_unsubscribe_fun(UnsubM) ->
    erlang:make_fun(UnsubM, unsubscribe, 2).
    
