%%%-----------------------------------------------------------------------------
%%%
%%% Desc:       xxxx
%%%
%%% Purpose:    xxxx
%%%
%%% Legal:      Copyright (c) 2011, The Authors.
%%%             http://github.com/SmokeAlarm
%%%             See LICENSE.txt for licensing information.
%%%
%%% Author:     Edmond Begumisa <ebegumisa at hysteria dash tech dot com>
%%%
%%%-----------------------------------------------------------------------------

-define(STRINGIFY(X), ??X).

%% CAVEAT: There are some evil things happening here! Use these only on records
%%         that are either...
%%         a) defined private to modules, or 
%%         b) if defined in .hrl files, are *intended* to be incompattible
%%            between versions (i.e. if the correct .hrl always accompannies
%%            .erl(s)|.beam(s) using these macros).

-define(PROPLIST_TO_RECORD(PropL, RecTag), 
        list_to_tuple([RecTag] ++ [proplists:get_value(K, PropL, undefined)
                                    || K <- record_info(fields, RecTag)])). % NB: K is a fresh generator variable (will generate a warning if shadowed)
                                    
-define(PROPLIST_FROM_RECORD(RecTerm, RecTag), 
        [{K, element(N, RecTerm)} || {N,K} <-  % NB: N and K are fresh generator variables (will generate a warning if shadowed)
            lists:zip(lists:seq(2, record_info(size, RecTag)),
                      record_info(fields, RecTag))]).

-define(RECORD_MAP(F, RecTerm, RecTag), 
        list_to_tuple([RecTag] ++
                      [F(K, element(N, RecTerm)) || {N,K} <- % NB: N and K are fresh generator variables (will generate a warning if shadowed)
                            lists:zip(lists:seq(2, record_info(size, RecTag)),
                                      record_info(fields, RecTag))])).

