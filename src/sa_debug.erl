%%%-----------------------------------------------------------------------------
%%%
%%% Desc:       Debugging Utilities
%%%
%%% Purpose:    - Provide useful debugging/tracing functions.
%%%             - Provide implementations for debugging macros in sa_debug.hrl
%%%
%%%-----------------------------------------------------------------------------

-module(sa_debug).
-legal("Copyright (c) 2011, The Authors.\n"
        "http://github.com/SmokeAlarm\n"
        "See LICENSE.txt for licensing information.").
-author("Edmond Begumisa <ebegumisa at hysteria dash tech dot com>").

%% Public Interface
-export([start_tracer/1, start_et_viewer/1, enable_mods_trace/3]).
-export([data_type/1]).

%% Private Interface
-export([et_report_event/9, et_report_event/10,
         parse_transform/2, format_desc/2, format_func/6, format_time/1]).

%% Private Prefuncess
-define(SA_DEBUG_NO_TRANSFORM, true).
-include("sa_debug.hrl"). 
-undef(SA_DEBUG_NO_TRANSFORM).
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("et/include/et.hrl").

%%% Interface ------------------------------------------------------------------

start_tracer(io) ->
    dbg:tracer();
start_tracer({TCPPort, QueSize}) when is_number(TCPPort) ->
    dbg:tracer(port, dbg:trace_port(ip, {TCPPort, QueSize}));
start_tracer(TCPPort) when is_number(TCPPort) ->
    dbg:tracer(port, dbg:trace_port(ip, TCPPort));
start_tracer({Path, FlushRate}) when is_integer(FlushRate) ->
    Started =   case (Ret = dbg:tracer(port, dbg:trace_port(file, Path))) of
                    {ok, _} ->
                        true;
                    {error, already_started} ->
                        true;
                    _ ->
                        false
                end,
    Par = list_to_atom(?MODULE_STRING "_flush_trace_port"),
    case application:get_env(kernel, Par) of
        undefined -> ok;
        {ok, TRef0} -> _ = timer:cancel(TRef0)
    end,
    if
        Started=:=true, FlushRate>0 ->
            {ok, TRef1} =
                timer:apply_interval(FlushRate, dbg, flush_trace_port, []),
            ok = application:set_env(kernel, Par, TRef1);
        true ->
            ok = application:unset_env(kernel, Par)
    end,
    Ret;
start_tracer({Path, {Suffix, WrapSize, WrapCnt}}) ->
    dbg:tracer(port, dbg:trace_port(file, {Path, wrap, Suffix,
                                           WrapSize, WrapCnt}));
start_tracer({Path, Suffix}) ->
    start_tracer({Path, {Suffix, 128*1024, 8}});
start_tracer(Path) ->
    dbg:tracer(port, dbg:trace_port(file, Path)).

start_et_viewer(undefined) ->
    do_start_et_viewer(undefined);
start_et_viewer({Hostname, TCPPort}) when is_number(TCPPort) ->
    do_start_et_viewer({ip,{Hostname,TCPPort}});
start_et_viewer(TCPPort) when is_number(TCPPort) ->
    do_start_et_viewer({ip,TCPPort});
start_et_viewer(Path) ->
    do_start_et_viewer({follow_file, Path}).

do_start_et_viewer(TraceClient) ->
    TracePattern = {et, max},
    NewFitlerAll = et_trace_me_filter(TracePattern),
    Opts =  [{title, "SmokeAlarm Replay"},
             {max_actors, 10},
             {trace_global, false},
             {trace_pattern, TracePattern},
             {dict_insert, {filter, all}, NewFitlerAll}
              | case TraceClient of
                    undefined -> [];
                    _ -> [{trace_client, TraceClient}]
                end],
    ReplayHandle =
        case Ret = et_viewer:start(Opts, gs) of % NB: et_wx_viewer has issues on windows (detail window can't get focus)
            {ok, VwrPid} -> et_viewer:get_collector_pid(VwrPid);
            _ -> undefined
        end,
    ok = application:set_env(smokealarm, replay_handle, ReplayHandle),
    Ret.

enable_mods_trace(Mods, trace_me, Enable) ->
    MS = [begin
            [MF] = dbg:fun2ms(
                    fun ([_,_,_,_,[{mfnl,X,_,_,_}|_]]) when X=:=Mod ->
                        true
                    end),
            MF
          end || Mod <- Mods],
    case Enable of
        true -> dbg:tp({et,trace_me,5}, MS);
        false -> dbg:ctpg({et,trace_me,5}) % TODO: Find out how to only disable the match spec
    end;
enable_mods_trace(Mods, calls, Enable) ->
    [{Mod,  case Enable of
                true ->
                    MF = {'_',[],[{return_trace},{exception_trace}]},
                    dbg:tpl({Mod,'_','_'}, [MF]);
                false ->
                    dbg:ctpl({Mod,'_','_'})
            end} || Mod <- Mods].

data_type(X) ->
    if
        is_atom(X)      -> atom;
        is_binary(X)    -> binary;
        is_bitstring(X) -> bitstring;
        is_boolean(X)   -> boolean;
        is_float(X)     -> float;
        is_function(X)  -> function;
        is_integer(X)   -> integer;
        is_list(X)      -> list;
        is_number(X)    -> number;
        is_pid(X)       -> pid;
        is_port(X)      -> port;
        is_reference(X) -> reference;
        is_tuple(X)     -> tuple
    end.
    
et_report_event(Handle0, Detail, From, To, Label, Module, Function, Arity, Line)
    ->
    {ok, Handle1} =
        et_collector:report_event(Handle0, Detail, From, To, Label,
                                  [{mfnl, Module, Function, Arity, Line}]),
    Handle1.
    
et_report_event(Handle0, Detail, From, To, Label, Content,
                Module, Function, Arity, Line)
    ->
    {ok, Handle1} =
        et_collector:report_event(Handle0, Detail, From, To, Label,
                                  [{mfnl, Module, Function, Arity, Line},
                                   {data, Content}]),
    Handle1.

parse_transform(AST, _Opts) ->
    [do_parse(T) || T <- AST]. 

do_parse({function, _FLine, FName, FArity, _FRepL} = T) ->
    erl_syntax_lib:map(
        fun (TE) ->
            do_parse_macro(FName, FArity, erlang:localtime(), TE)
        end, T); 
do_parse(T) ->
    T.

do_parse_macro(FName, FArity, CTime, TE) -> 
    T = case atom =:= erl_syntax:type(TE) andalso erl_syntax:atom_value(TE) of 
            ?FUNCTION ->
                erl_syntax:atom(FName); 
            ?ARITY ->
                erl_syntax:integer(FArity);
            ?COMPILE_TIME ->
                erl_syntax:string(format_time(CTime));
            _ ->
                TE
        end,
    erl_syntax:revert(T).

format_desc(Fmt, Args) ->
    try io_lib:format(Fmt, Args) % Bad formatting in ?info*|?warning*|?error* macros shouldn't crash process
    catch error:Why ->
        io_lib:format("<BAD FORMAT (~p, ~p): ~p>", [Fmt, Args, Why])
    end.

format_func(App, Mod, Func, Arity, File, Line) ->
    {ok, ProgName} = init:get_argument(progname),
    io_lib:format("~s:~w: (~s!~w::~w:~w/~w)",
                  [File, Line, ProgName, App, Mod, Func, Arity]).

%% Modified from inets httpd_utils:rfc1123_date()

format_time({{YYYY, MM, DD}, {Hour, Min, Sec}}) ->
    DayNumber = calendar:day_of_the_week({YYYY, MM, DD}),
    lists:flatten(
      io_lib:format("~s, ~2.2.0w ~3.s ~4.4.0w ~2.2.0w:~2.2.0w:~2.2.0w",
		    [day(DayNumber), DD, month(MM), YYYY, Hour, Min, Sec])).

%%% Helpers --------------------------------------------------------------------


day(1) -> "Mon";
day(2) -> "Tue";
day(3) -> "Wed";
day(4) -> "Thu";
day(5) -> "Fri";
day(6) -> "Sat"; 
day(7) -> "Sun".

month(1) -> "Jan";
month(2) -> "Feb";
month(3) -> "Mar";
month(4) -> "Apr";
month(5) -> "May";
month(6) -> "Jun";
month(7) -> "Jul";
month(8) -> "Aug";
month(9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

et_trace_me_filter(TracePattern) ->
    {Mod, _} = et_selector:make_pattern(TracePattern),
    fun % Discard all native trace events except for et:trace_me
        ({trace,_,call,{et,trace_me,_}}=E) ->
            % Convert OTP behaviour {Pid,RefTag} to regular Pid
            case et_selector:parse_event(Mod, E) of 
                {true, #event{from={FrP,FrR},to={ToP,ToR}}=ERec} when
                    is_pid(FrP), is_reference(FrR),
                    is_pid(ToP), is_reference(ToR)
                ->
                    {true, ERec#event{from=FrP,to=ToP}};
                {true, #event{from={FrP,FrR}}=ERec} when
                    is_pid(FrP), is_reference(FrR)
                ->
                    {true, ERec#event{from=FrP}};
                {true, #event{to={ToP,ToR}}=ERec} when
                   is_pid(ToP), is_reference(ToR)
                ->
                    {true, ERec#event{to=ToP}};
                Other ->
                    Other
            end;
        ({trace,_,_,_})         -> false;
        ({trace,_,_,_,_})       -> false;
        ({trace_ts,_,_,_,_})    -> false;
        ({trace_ts,_,_,_,_,_})  -> false;
        ({seq_trace,_,_})       -> false;
        ({seq_trace,_,_,_})     -> false;
        ({drop,_})              -> false;
        (E) ->
            et_selector:parse_event(Mod, E)
    end.
