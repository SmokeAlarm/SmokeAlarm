%%%-----------------------------------------------------------------------------
%%%
%%% Desc:       Common include
%%%
%%% Purpose:    Provide commonly used macros/records in a single hrl that all
%%%             modules include.
%%%
%%% Legal:      Copyright (c) 2011, The Authors.
%%%             http://github.com/SmokeAlarm
%%%             See LICENSE.txt for licensing information.
%%%
%%% Author:     Edmond Begumisa <ebegumisa at hysteria dash tech dot com>
%%%
%%%-----------------------------------------------------------------------------

%% Conditional compilation
-define(CON_DEBUG, true).

%% Global macros/records
-include("../include/smokealarm_conf.hrl").
-include("sa_utils.hrl").
-define(APP, smokealarm).
-define(APP_STRING, STRINGIFY(APP)).
-include("sa_debug.hrl").

