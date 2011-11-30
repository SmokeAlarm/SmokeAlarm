%%%-----------------------------------------------------------------------------
%%%
%%% Desc:       Debugging Macros
%%%
%%% Purpose:    - Provide ?FUNCTION, ?ARITY and other small macros
%%%             - Provide ?info/?error/?warning macro replacements for
%%%               error_logger funcs
%%%               NB: Macro versions add funcedure info (file,line,etc) and use
%%%                   a format that SmokeAlarm can recognise so it doesn't get
%%%                   itself into infinate event loops (see sa_std_feed_errlog)
%%%             - Provide high-level tracing macros for use with et.
%%%
%%% Legal:      Copyright (c) 2011, The Authors.
%%%             http://github.com/SmokeAlarm
%%%             See LICENSE.txt for licensing information.
%%%
%%% Author:     Edmond Begumisa <ebegumisa at hysteria dash tech dot com>
%%%
%%%-----------------------------------------------------------------------------

-ifndef(SA_DEBUG_NO_TRANSFORM).
    -compile([{parse_transform, sa_debug}]).
-endif.

%% Replaced with the calling function's arity integer()
-define(ARITY, '__function_arity__').       

%% Replaced with the calling function as an atom
-define(FUNCTION, '__function_macro__').

%% Replaced with compile time (atom of same format as erlang:localtime/0)
-define(COMPILE_TIME, '__compile_time___').

%% These are not intended to be used directly.
%% Use ?trace_me, ?trace_me_IF or ?trace_me_else instead.

-define(do_trace_me(Detail, From, To, Label),
    et:trace_me(Detail, From, To, Label,
                [{mfnl, ?MODULE, ?FUNCTION, ?ARITY, ?LINE}])).
-define(do_trace_me(Detail, From, To, Label, Content),
    et:trace_me(Detail, From, To, Label,
                [{mfnl, ?MODULE, ?FUNCTION, ?ARITY, ?LINE},
                 {data, Content}])).
-define(do_trace_me_if(Expr, Detail, SameFromTo, Label),
    (case true == (Expr) of
        true -> ?do_trace_me(Detail, SameFromTo, SameFromTo, Label);
        false -> ok
     end)).
-define(do_trace_me_if(Expr, Detail, From, To, Label),
    (case true == (Expr) of
        true -> ?do_trace_me(Detail, From, To, Label);
        false -> ok
     end)).
-define(do_trace_me_if(Expr, Detail, From, To, Label, Content),
    (case true == (Expr) of
        true -> ?do_trace_me(Detail, From, To, Label, Content);
        false -> ok
     end)).
-define(do_trace_me_else(Expr, Detail, From, To,
                         IfLabel, IfContent,
                         ElseLabel, ElseContent),
    (case true == (Expr) of
        true -> ?do_trace_me(Detail, From, To, IfLabel, IfContent);
        false -> ?do_trace_me(Detail, From, To, ElseLabel, ElseContent)
     end)).
-define(do_trace_me_else(Expr, Detail, 
                         IfFrom, IfTo, IfLabel, IfContent,
                         ElseFrom, ElseTo, ElseLabel, ElseContent),
    (case true == (Expr) of
        true -> ?do_trace_me(Detail, IfFrom, IfTo, IfLabel, IfContent);
        false -> ?do_trace_me(Detail, ElseFrom, ElseTo, ElseLabel, ElseContent)
     end)).

%% These are intended for "high-level" tracing with et:viewer to allow "markers"
%% to be placed around code. They should cost next to nothing when the
%% et:trace_me/5 function is *not* being traced, so don't pass elaborate
%% expensive-to-construct terms to ?trace_me unless they are being used
%% anyway (i.e. don't create large terms specially for ?trace_me). 
-ifdef(TRACE_ME_DETAIL).
    -define(trace_me(SameFromTo, Label),
        ?do_trace_me(?TRACE_ME_DETAIL, SameFromTo, SameFromTo, Label)).
    -define(trace_me(From, To, Label),
        ?do_trace_me(?TRACE_ME_DETAIL, From, To, Label)).
    -define(trace_me(From, To, Label, Content),
        ?do_trace_me(?TRACE_ME_DETAIL, From, To, Label, Content)).
    -define(trace_me_IF(Expr, SameFromTo, Label),
        ?do_trace_me_if(Expr, ?TRACE_ME_DETAIL, SameFromTo, SameFromTo, Label)).
    -define(trace_me_IF(Expr, From, To, Label),
        ?do_trace_me_if(Expr, ?TRACE_ME_DETAIL, From, To, Label)).
    -define(trace_me_IF(Expr, From, To, Label, Content),
        ?do_trace_me_if(Expr, ?TRACE_ME_DETAIL, From, To, Label, Content)).
    -define(trace_me_else(Expr, From, To,
                          IfLabel, IfContent,
                          ElseLabel, ElseContent),
        ?do_trace_me_else(Expr, ?TRACE_ME_DETAIL, From, To,
                          IfLabel, IfContent,
                          ElseLabel, ElseContent)).
    -define(trace_me_else(Expr,
                          IfFrom, IfTo, IfLabel, IfContent,
                          ElseFrom, ElseTo, ElseLabel, ElseContent),
        ?do_trace_me_else(Expr, ?TRACE_ME_DETAIL,
                          IfFrom, IfTo, IfLabel, IfContent,
                          ElseFrom, ElseTo, ElseLabel, ElseContent)).
-else.
    -define(trace_me(Detail, SameFromTo, Label),
        ?do_trace_me(Detail, SameFromTo, SameFromTo, Label)).
    -define(trace_me(Detail, From, To, Label),
        ?do_trace_me(Detail, From, To, Label)).
    -define(trace_me(Detail, From, To, Label, Content),
        ?do_trace_me(Detail, From, To, Label, Content)).
    -define(trace_me_IF(Expr, Detail, SameFromTo, Label),
        ?do_trace_me_if(Expr, Detail, SameFromTo, SameFromTo, Label)).
    -define(trace_me_IF(Expr, Detail, From, To, Label),
        ?do_trace_me_if(Expr, Detail, From, To, Label)).
    -define(trace_me_IF(Expr, Detail, From, To, Label, Content),
        ?do_trace_me_if(Expr, Detail, From, To, Label, Content)).
    -define(trace_me_else(Expr, Detail, From, To,
                          IfLabel, IfContent,
                          ElseLabel, ElseContent),
        ?do_trace_me_else(Expr, Detail, From, To,
                          IfLabel, IfContent,
                          ElseLabel, ElseContent)).
    -define(trace_me_else(Expr, Detail, 
                          IfFrom, IfTo, IfLabel, IfContent,
                          ElseFrom, ElseTo, ElseLabel, ElseContent),
        ?do_trace_me_else(Expr, Detail,
                          IfFrom, IfTo, IfLabel, IfContent,
                          ElseFrom, ElseTo, ElseLabel, ElseContent)).
-endif.

%% These are not intended to be used directly.
%% Use ?report_me, ?report_me_if or ?report_me_else instead.

-define(do_report_me(Handle0, Detail, From, To, Label),
        (case Handle0 of
            undefined ->
                undefined;
            _ ->
                sa_debug:et_report_event(Handle0, Detail, From, To, Label,
                                         ?MODULE, ?FUNCTION, ?ARITY, ?LINE)
         end)).
-define(do_report_me(Handle0, Detail, From, To, Label, Content),
        (case Handle0 of
            undefined ->
                undefined;
            _ ->
                sa_debug:et_report_event(
                    Handle0, Detail, From, To, Label, Content,
                    ?MODULE, ?FUNCTION, ?ARITY, ?LINE)
         end)).
-define(do_report_me_if(Expr, Handle0, Detail, SameFromTo, Label),
    (case true == (Expr) of
        true -> ?do_report_me(Handle0, Detail, SameFromTo, Label);
        false -> Handle0
     end)).
-define(do_report_me_if(Expr, Handle0, Detail, From, To, Label),
    (case true == (Expr) of
        true -> ?do_report_me(Handle0, Detail, From, To, Label);
        false -> Handle0
     end)).
-define(do_report_me_if(Expr, Handle0, Detail, From, To, Label, Content),
    (case true == (Expr) of
        true -> ?do_report_me(Handle0, Detail, From, To, Label, Content);
        false -> Handle0
     end)).
-define(do_report_me_else(Expr, Handle0, Detail, From, To,
                          IfLabel, IfContent,
                          ElseLabel, ElseContent),
    (case true == (Expr) of
        true ->
            ?do_report_me(Handle0, Detail, From, To, IfLabel, IfContent);
        false ->
            ?do_report_me(Handle0, Detail, From, To, ElseLabel, ElseContent)
     end)).
-define(do_report_me_else(Expr, Handle0, Detail, 
                          IfFrom, IfTo, IfLabel, IfContent,
                          ElseFrom, ElseTo, ElseLabel, ElseContent),
    (case true == (Expr) of
        true ->
            ?do_report_me(Handle0, Detail, IfFrom, IfTo, IfLabel, IfContent);
        false ->
            ?do_report_me(
                Handle0, Detail, ElseFrom, ElseTo, ElseLabel, ElseContent)
     end)).

%% These are similar to ?trace_me* macros but markers are recorded by et
%% REGARDLESS of whether tracing is switched on or off. Instead...
%% - To turn on ?report_me* macros, pass a handle set an et_collector pid
%%   (usually belonging to an et_viewer) or a continuation (see:
%%   et_collector:report_event.) 
%% - To turn ?report_me* macros off, pass 'undefined' as the handle.
%% The idea is to allow optional ?trace_me*-like viewing without the overhead of
%% tracing (i.e. provide something suitable for use all the time in production).
%% The trade-off is that ?report_me macros (et_collector:report_event/6) are not
%% as light-weight as ?trace_me* macros (et:trace_me/5) when NOT switched on and
%% should be used for "major" markers. In other words, ?report_me macros ON are
%% much lighter than ?trace_me macros ON (because the latter requires tracing)
%% *BUT* ?report_me macros OFF are heavier than ?trace_me macros OFF.
-ifdef(TRACE_ME_DETAIL). % Intentional
    -define(report_me(Handle0, SameFromTo, Label),
        ?do_report_me(Handle0, ?TRACE_ME_DETAIL, SameFromTo, Label)).
    -define(report_me(Handle0, From, To, Label),
        ?do_report_me(Handle0, ?TRACE_ME_DETAIL, From, To, Label)).
    -define(report_me(Handle0, From, To, Label, Content),
        ?do_report_me(Handle0, ?TRACE_ME_DETAIL, From, To, Label, Content)).
    -define(report_me_if(Expr, Handle0, SameFromTo, Label),
        ?do_report_me_if(Expr, Handle0, ?TRACE_ME_DETAIL, SameFromTo, Label)).
    -define(report_me_if(Expr, Handle0, From, To, Label),
        ?do_report_me_if(Expr, Handle0, ?TRACE_ME_DETAIL, From, To, Label)).
    -define(report_me_if(Expr, Handle0, From, To, Label, Content),
        ?do_report_me_if(
            Expr, Handle0, ?TRACE_ME_DETAIL, From, To, Label, Content)).
    -define(report_me_else(Expr, Handle0, From, To,
                           IfLabel, IfContent,
                           ElseLabel, ElseContent),
        ?do_report_me_else(Expr, Handle0, ?TRACE_ME_DETAIL, From, To,
                           IfLabel, IfContent,
                           ElseLabel, ElseContent)).
    -define(report_me_else(Expr,
                           IfFrom, IfTo, IfLabel, IfContent,
                           ElseFrom, ElseTo, ElseLabel, ElseContent),
        ?do_report_me_else(Expr, Handle0, ?TRACE_ME_DETAIL,
                           IfFrom, IfTo, IfLabel, IfContent,
                           ElseFrom, ElseTo, ElseLabel, ElseContent)).
-else.
    -define(report_me(Handle0, Detail, SameFromTo, Label),
        ?do_report_me(Handle0, Detail, SameFromTo, SameFromTo, Label)).
    -define(report_me(Handle0, Detail, From, To, Label),
        ?do_report_me(Handle0, Detail, From, To, Label)).
    -define(report_me(Handle0, Detail, From, To, Label, Content),
        ?do_report_me(Handle0, Detail, From, To, Label, Content)).
    -define(report_me_if(Expr, Handle0, Detail, SameFromTo, Label),
        ?do_report_me_if(Expr, Handle0, Detail, SameFromTo, SameFromTo, Label)).
    -define(report_me_if(Expr, Handle0, Detail, From, To, Label),
        ?do_report_me_if(Expr, Handle0, Detail, From, To, Label)).
    -define(report_me_if(Expr, Handle0, Detail, From, To, Label, Content),
        ?do_report_me_if(Expr, Handle0, Detail, From, To, Label, Content)).
    -define(report_me_else(Expr, Handle0, Detail, From, To,
                           IfLabel, IfContent,
                           ElseLabel, ElseContent),
        ?do_report_me_else(Expr, Handle0, Detail, From, To,
                           IfLabel, IfContent,
                           ElseLabel, ElseContent)).
    -define(report_me_else(Expr, Handle0, Detail, 
                           IfFrom, IfTo, IfLabel, IfContent,
                           ElseFrom, ElseTo, ElseLabel, ElseContent),
        ?do_report_me_else(Expr, Handle0, Detail,
                           IfFrom, IfTo, IfLabel, IfContent,
                           ElseFrom, ElseTo, ElseLabel, ElseContent)).
-endif.

%% These are not intended to be used directly.
%% Use ?error or ?error_if instead.

-define(do_error(Mod, Func, Arity, File, Line, Tag, DescFmt, DescArgs, Data),
    erlang:error({Tag,
                    {desc, lists:flatten(
                                sa_debug:format_desc(DescFmt,DescArgs))},
                    {data, Data},
                    {mfn, Mod, Func, Arity},
                    {fl, File, Line}})).

-define(do_error(Mod, Func, Arity, File, Line, Tag, DescFmt, DescArgs),
    erlang:error({Tag,
                    {desc, lists:flatten(
                                sa_debug:format_desc(DescFmt,DescArgs))},
                    {mfn, Mod, Func, Arity},
                    {fl, File, Line}})).
     
%% Use instead of erlang:error.
%% If used outside of SmokeAlarm ENSURE you define a different ?APP macro.

-define(error(Tag, DescFmt, DescArgs, Data),
        ?do_error(?MODULE, ?FUNCTION, ?ARITY, ?FILE, ?LINE,
                  Tag, DescFmt, DescArgs, Data)).

-define(error(Tag, DescFmt, DescArgs),
        ?do_error(?MODULE, ?FUNCTION, ?ARITY, ?FILE, ?LINE,
                  Tag, DescFmt, DescArgs)).

-define(error(Tag, DescFmt),
        ?do_error(?MODULE, ?FUNCTION, ?ARITY, ?FILE, ?LINE,
                  Tag, DescFmt, [])).

-define(error(DescFmt),
        ?do_error(?MODULE, ?FUNCTION, ?ARITY, ?FILE, ?LINE,
                 untagged, DescFmt, [])).

-define(error_if(Expr, Tag, DescFmt, DescArgs, Data),
        (case true == (Expr) of
            false -> ok;
            true -> ?error(Tag, DescFmt, DescArgs, Data)
         end)).

-define(error_if(Expr, Tag, DescFmt, DescArgs),
        (case true == (Expr) of
            false -> ok;
            true -> ?error(Tag, DescFmt, DescArgs)
         end)).

-define(error_if(Expr, Tag, DescFmt),
        (case true == (Expr) of
            false -> ok;
            true -> ?error(Tag, DescFmt)
         end)).

-define(error_if(Expr, DescFmt),
        (case true == (Expr) of
            false -> ok;
            true -> ?error(DescFmt)
         end)).

%% These are not intended to be used directly.
%% Use ?warning, ?warning_if or ?info_else_warning instead.

-define(do_warning(Mod, Func, Arity, File, Line, Tag, DescFmt, DescArgs, Data),
    (begin
        error_logger:warning_report(
            [{?APP, Tag},
             {desc, lists:flatten(sa_debug:format_desc(DescFmt,DescArgs))},
             {data, Data},
             {func, lists:flatten(
                        sa_debug:format_func(?APP,Mod,Func,Arity,File,Line))},
             {node, erlang:node()}]),
        case error_logger:warning_map() of
            error ->
                ?do_error(Mod, Func, Arity, File, Line, Tag, DescFmt, DescArgs,
                          Data);
            _ ->
                ok
        end
     end)).

-define(do_warning(Mod, Func, Arity, File, Line, Tag, DescFmt, DescArgs),
    (begin
        error_logger:warning_report(
            [{?APP, Tag},
             {desc, lists:flatten(sa_debug:format_desc(DescFmt,DescArgs))},
             {func, lists:flatten(
                        sa_debug:format_func(?APP,Mod,Func,Arity,File,Line))},
             {node, erlang:node()}]),
        case error_logger:warning_map() of
            error ->
                ?do_error(Mod, Func, Arity, File, Line, Tag, DescFmt, DescArgs);
            _ ->
                ok
        end
     end)).
     
%% Use instead of error_logger:warning_msg|error_logger:warning_report.
%% If used outside of SmokeAlarm ENSURE you define a different ?APP macro.

-define(warning(Tag, DescFmt, DescArgs, Data),
        ?do_warning(?MODULE, ?FUNCTION, ?ARITY, ?FILE, ?LINE,
                    Tag, DescFmt, DescArgs, Data)).

-define(warning(Tag, DescFmt, DescArgs),
        ?do_warning(?MODULE, ?FUNCTION, ?ARITY, ?FILE, ?LINE,
                    Tag, DescFmt, DescArgs)).

-define(warning(Tag, DescFmt),
        ?do_warning(?MODULE, ?FUNCTION, ?ARITY, ?FILE, ?LINE,
                    Tag, DescFmt, [])).
                    
-define(warning(DescFmt),
        ?do_warning(?MODULE, ?FUNCTION, ?ARITY, ?FILE, ?LINE,
                    untagged, DescFmt, [])).

-define(warning_if(Expr, Tag, DescFmt, DescArgs, Data),
        (case true == (Expr) of
            false -> ok;
            true -> ?warning(Tag, DescFmt, DescArgs, Data)
         end)).

-define(warning_if(Expr, Tag, DescFmt, DescArgs),
        (case true == (Expr) of
            false -> ok;
            true -> ?warning(Tag, DescFmt, DescArgs);
         end)).

-define(warning_if(Expr, Tag, DescFmt),
        (case true == (Expr) of
            false -> ok;
            true -> ?warning(Tag, DescFmt)
         end)).

-define(warning_if(Expr, DescFmt),
        (case true == (Expr) of
            false -> ok;
            true -> ?warning(DescFmt)
         end)).

%% These are not intended to be used directly.
%% Use ?info, ?info_if or ?info_else_warning instead

-define(do_info(Mod, Func, Arity, File, Line, Tag, DescFmt, DescArgs, Data),
    (error_logger:info_report(
            [{?APP, Tag},
             {desc, lists:flatten(sa_debug:format_desc(DescFmt,DescArgs))},
             {data, Data},
             {func, lists:flatten(
                        sa_debug:format_func(?APP,Mod,Func,Arity,File,Line))},
             {node, erlang:node()}]))).

-define(do_info(Mod, Func, Arity, File, Line, Tag, DescFmt, DescArgs),
    (error_logger:info_report(
            [{?APP, Tag},
             {desc, lists:flatten(sa_debug:format_desc(DescFmt,DescArgs))},
             {func, lists:flatten(
                        sa_debug:format_func(?APP,Mod,Func,Arity,File,Line))},
             {node, erlang:node()}]))).
    
%% Use instead of error_logger:info_msg|error_logger:info_report.
%% If used outside of SmokeAlarm ENSURE you define a different ?APP macro.

-define(info(Tag, DescFmt, DescArgs, Data),
        ?do_info(?MODULE, ?FUNCTION, ?ARITY, ?FILE, ?LINE,
                 Tag, DescFmt, DescArgs, Data)).

-define(info(Tag, DescFmt, DescArgs),
        ?do_info(?MODULE, ?FUNCTION, ?ARITY, ?FILE, ?LINE,
                 Tag, DescFmt, DescArgs)).

-define(info(Tag, DescFmt),
        ?do_info(?MODULE, ?FUNCTION, ?ARITY, ?FILE, ?LINE,
                 Tag, DescFmt, [])).

-define(info(DescFmt),
        ?do_info(?MODULE, ?FUNCTION, ?ARITY, ?FILE, ?LINE,
                 untagged, DescFmt, [])).

-define(info_if(Expr, Tag, DescFmt, DescArgs, Data),
        (case true == (Expr) of
            false -> ok;
            true -> ?info(Tag, DescFmt, DescArgs, Data)
         end)).

-define(info_if(Expr, Tag, DescFmt, DescArgs),
        (case true == (Expr) of
            false -> ok;
            true -> ?info(Tag, DescFmt, DescArgs)
         end)).

-define(info_if(Expr, Tag, DescFmt),
        (case true == (Expr) of
            false -> ok;
            true -> ?info(Tag, DescFmt)
         end)).

-define(info_if(Expr, DescFmt),
        (case true == (Expr) of
            false -> ok;
            true -> ?info(DescFmt)
         end)).

%% Use to show info if something succeeds otherwise raise a warning if it fails.

-define(info_else_warning(Expr, Tag, DescFmt, DescArgs, Data),
        (case true == (Expr) of
            true -> ?info(Tag, DescFmt, DescArgs, Data);
            false -> ?warning(Tag, DescFmt, DescArgs, Data)
         end)).

-define(info_else_warning(Expr, Tag, DescFmt, DescArgs),
        (case true == (Expr) of
            true -> ?info(Tag, DescFmt, DescArgs);
            false -> ?warning(?Tag, DescFmt, DescArgs)
         end)).

-define(info_else_warning(Expr, Tag, DescFmt),
        (case true == (Expr) of
            true -> ?info(Tag, DescFmt, []);
            false -> ?warning(Tag, DescFmt, [])
         end)).
