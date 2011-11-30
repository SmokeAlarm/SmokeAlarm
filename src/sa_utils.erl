%%%-----------------------------------------------------------------------------
%%%
%%% Desc:       General purpose utilities.
%%%
%%% Purpose:    Make SmokeAlarm development a little easier.
%%%
%%%-----------------------------------------------------------------------------

-module(sa_utils).
-legal("Copyright (c) 2011, The Authors.\n"
        "http://github.com/SmokeAlarm\n"
        "See LICENSE.txt for licensing information.").
-author("Edmond Begumisa <ebegumisa at hysteria dash tech dot com>").

%% Public Interface
-export([]).

%% Private Interface
-export([lists_foldl_while/3, lists_drop_while_folding/3]).
-export([queue_fold_while/3, queue_drop_while/2,
         queue_drop_while_folding/3, queue_map_while/3]).
-export([proplists_subtract/2, proplists_umerge/2]).
-export([binary_ordered_match/2]).
-export([to_valid_utf8_bin/1, to_utf8_bin/1,
         to_valid_utf8_list/1, to_utf8_list/1]).
         
%% Private Preprocess
-include("common.hrl").

%%% Interface ------------------------------------------------------------------

%% this is a comment.

%% Similar to lists:foldl except F must return false|{true|false, AccOut}
%% where if the value is false or first element of tuple is false then
%% traversal is abandoned.

lists_foldl_while(F, AccIn, [H|T]) ->
    case F(H, AccIn) of
        {true, AccOut} -> lists_foldl_while(F, AccOut, T);
        {false, AccOut} -> AccOut;
        false -> AccIn
    end;
lists_foldl_while(F, Acc, []) when is_function(F, 2) ->
    Acc.

lists_drop_while_folding(F, AccIn, [H|T]=L0) ->
    case F(H, AccIn) of
        {true, AccOut} -> lists_drop_while_folding(F, AccOut, T);
        {false, AccOut} -> {L0, AccOut};
        false -> {L0, AccIn}
    end;
lists_drop_while_folding(F, AccIn, []=L0) when is_function(F, 2) ->
    {L0, AccIn}.
    
queue_fold_while(F, AccIn, Q0) ->
    case queue:out(Q0) of
        {{value, H}, Q1} ->
            case F(H, AccIn) of
                {true, AccOut} -> queue_fold_while(F, AccOut, Q1);
                {false, AccOut} -> AccOut;
                false -> AccIn 
            end;
        {empty, _} when is_function(F, 2) ->
            AccIn
    end.

queue_drop_while(Pred, Q0) ->
    case queue:out(Q0) of
        {{value, H}, Q1} ->
            case Pred(H) of
                true -> queue_drop_while(Pred, Q1);
                false -> Q0
            end;
        {empty, _} when is_function(Pred, 1) -> 
            Q0
    end.

queue_drop_while_folding(F, AccIn, Q0) ->
    case queue:out(Q0) of
        {{value, H}, Q1} ->
            case F(H, AccIn) of
                {true, AccOut} -> queue_drop_while_folding(F, AccOut, Q1);
                {false, AccOut} -> {Q0, AccOut};
                false -> {Q0, AccIn}
            end;
        {empty, _} when is_function(F, 2) ->
            {Q0, AccIn}
    end.

queue_map_while(F, AccIn, Q) ->
    do_queue_map_while(F, AccIn, Q, queue:new()).
    
do_queue_map_while(F, AccIn, QTake0, QPut) ->
    case queue:out(QTake0) of
        {{value, HIn}, QTake1} ->
            case F(HIn, AccIn) of
                {true, HOut, AccOut} ->
                    do_queue_map_while(F, AccOut, QTake1, queue:in(HOut, QPut));
                {false, AccOut} ->
                    {queue:join(QPut, QTake1), AccOut};
                false ->
                    {queue:join(QPut, QTake0), AccIn}
            end;
        {empty, _} when is_function(F, 2) ->
            {queue:join(QPut, QTake0), AccIn}
    end.

%% Similar to lists:subtract/2 (with same caveat) but works on property lists
%% keys ignoring values.
proplists_subtract([], PropsB) when is_list(PropsB) ->
    [];
proplists_subtract(PropsA, []) when is_list(PropsA) ->
    PropsA;
proplists_subtract(PropsA, PropsB) ->
    [A || A <- PropsA,
            not lists:any(
                    fun
                        ({B,_}) ->
                            case A of
                                {B,_} -> true;
                                B -> true;
                                _ -> false
                            end;
                        (B) ->
                            case A of
                                {B,_} -> true;
                                B -> true;
                                _ -> false
                            end    
                    end, PropsB)].

%% Combine proplists A and B removing duplicates.
%% - If any defined in both A and B, then the one in A is taken
%% - Single atom items are converted to {Atom, true}
proplists_umerge(PropsA, PropsB) ->
    lists:ukeymerge(1, lists:ukeysort(1, proplists:unfold(PropsA)),
                       lists:ukeysort(1, proplists:unfold(PropsB))).
                       

binary_ordered_match(Bin, []) when is_binary(Bin) ->
    [];
binary_ordered_match(Bin, [H|T]) ->
    case binary:match(Bin, H) of
        nomatch -> [];
        Found -> do_binary_ordered_match(Bin, byte_size(Bin), T, [Found])
    end.
    
do_binary_ordered_match(_, _, [], Acc) ->
    lists:reverse(Acc);
do_binary_ordered_match(Bin, Size, [H|T], [{PrevPos,PrevLen}|_]=Acc) ->
    case binary:match(
            Bin, H, [{scope, {PrevPos+PrevLen, Size-PrevPos-PrevLen}}])
    of
        nomatch -> Acc;
        Found -> do_binary_ordered_match(Bin, Size, T, [Found|Acc])
    end.

to_valid_utf8_bin(BorL) ->
    case
        try unicode:characters_to_binary(BorL, unicode, unicode)
        catch _:Why -> {error, Why}
        end
    of
        B when is_binary(B) ->
            {ok, B};
        UCFaultTup ->
            {error, UCFaultTup}
    end.

to_utf8_bin(BorL) ->
    {ok, B} = to_valid_utf8_bin(BorL),
    B.
    
to_valid_utf8_list(BorL) ->
    case
        try unicode:characters_to_list(BorL, unicode)
        catch _:Why -> {error, Why}
        end
    of
        L when is_list(L) ->
            {ok, L};
        UCFaultTup ->
            {error, UCFaultTup}
    end.

to_utf8_list(BorL) ->
    {ok, L} = to_valid_utf8_list(BorL),
    L.