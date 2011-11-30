%%%-----------------------------------------------------------------------------
%%%
%%% Desc:       Predefined SmokeAlarm AppMon Information Feed.
%%%
%%% Purpose:    TODO: Use the same backend as AppMon to avail the same kind of
%%%             events appmon is able to detect including...
%%%             - Node load
%%%             - Existing/started/stopped OTP applications
%%%             - Existing/started/exited processes and thier relations (poll
%%%               based)
%%%
%%%-----------------------------------------------------------------------------

-module(sa_stdfeed_appmoni).
-legal("Copyright (c) 2011, The Authors.\n"
        "http://github.com/SmokeAlarm\n"
        "See LICENSE.txt for licensing information.").
-author("Edmond Begumisa <ebegumisa at hysteria dash tech dot com>").

%% Public Interface
-export([subscribe/2, unsubscribe/2]).

%% Private Interface
-export([]).

%% Private Preprocess
-import(smokealarm, [report_event/2, report_event/3,
                     report_multi_simul_events/2]).

%%% Interface ------------------------------------------------------------------

subscribe(_SubscriberId, OptsL) when is_list(OptsL) ->
    {ok, todo}.
        
unsubscribe(_SubscriberId, todo) ->
    ok. 

