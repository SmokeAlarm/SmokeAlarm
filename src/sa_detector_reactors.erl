%%%-----------------------------------------------------------------------------
%%%
%%% Desc:       KV Store of registered reactors
%%%
%%% Purpose:    - Handle requests sent to a detector to manage it's dictionary
%%%               of reactors.
%%%             - Handle callbacks to reactors.
%%%             - Reduce clutter in sa_detector.erl and make detectors easier to
%%%               trace by separating out reactor management/callback code.
%%%             
%%% Note:       1. This module, like it's owner (sa_detector.erl) is defensively
%%%                programmed because detectors are "kernels" that shouldn't be
%%%                crashed by reactor management requests or reactor callbacks.
%%%                Instead, faulty requests are rejected (possibly crashing the
%%%                reqestor) while fautly reactors are either rejected early or
%%%                automatically removed later (with appropriate warnings.)
%%%             2. Like gen_event handlers, reactors are not in their own
%%%                processes because reactors need be able to be called
%%%                sequentially...
%%%                     e.g. log error THEN send e-mail THEN reboot OS.
%%%                Unlike gen_event handlers, reactors are not immediately
%%%                removed if their callbacks fail (though they always generate
%%%                warnings). Instead, like supervised processes, faulty
%%%                reactors are only automatically removed if they fail too
%%%                frequently (which also generates warnings.)
%%%             3. If a reactor callback needs to perform a potentially long
%%%                running operation in a non-blocking manner (like sending
%%%                e-mail), it can create a linked process then return ok.
%%%                The linked process will generate a warning in the detector
%%%                if it crashes
%%%
%%%                SIDE: If the +W w emulator arg is not used, the warnings
%%%                      will be a crashes instead.
%%%
%%%-----------------------------------------------------------------------------

-module(sa_detector_reactors).
-legal("Copyright (c) 2011, The Authors.\n"
        "http://github.com/SmokeAlarm\n"
        "See LICENSE.txt for licensing information.").
-author("Edmond Begumisa <ebegumisa at hysteria dash tech dot com>").

%% Public Interface
-export([new/0]).
-export([add_reactor/4, add_reactors/4, add_reactors_atomic/4,
         remove_reactor/4, remove_reactors/4, remove_reactors_atomic/4,
         remove_all_reactors/3,
         which_reactors/3]).

%% Private Interface
-export([call_all_reactors/4]).

%% Private Preprocess
-define(TRACE_ME_DETAIL, 30). % Above detector, same as feeds.
-include("common.hrl").

-record(reactor, {  % What to store in a dictionary entry
            tripped_cb,         % do_nothing|#callback{} -- what to do when the
                                %   owner detector trips
            reset_cb,           % do_nothing|#callback{} -- what to do when the
                                %   owner detector resets
            first_cb_err_ts,    % erlang:now -- timestamp of first call to a
                                %   tripped_cb.'fun' or a reset_cb.'fun' that
                                %   generated an error or returned =/= ok.
            cb_err_cnt=0        % No. of times either callback has generated an
                                %   error or returned =/= ok (if too many occur 
        }).                     %   in configured period, reactor is removed)
        
-record(callback, { % What to call when owner detector trips|resets
            'fun',      % fun() -- The callback 
            args,       % term() -- Additional arg to pass to fun
            fun_info    % erlang:fun_info of fun -- useful for debugging
        }).
%%% Interface ------------------------------------------------------------------

new() ->
    dict:new().

add_reactor({K, Spec}, ReqFrm, DetK, D0) ->
    ?trace_me(ReqFrm, DetK, ?FUNCTION, {K,Spec}),
    Ret = {{What,_}=Reply, _} =
        case (dict:is_key(K, D0)
              orelse
                try make_reactor(Spec)
                catch _:Why -> {error, {'reactor_spec',Why,{K,Spec}}}
                end)
        of
            true ->
                {{error,{already_exists,K}}, D0};
            {error,_}=Err ->
                {Err, D0};
            #reactor{tripped_cb=TrippedCB,reset_cb=ResetCB}=Rtor ->
                {{ok,{K,{TrippedCB,ResetCB}}}, dict:store(K, Rtor, D0)}
        end,
    ?trace_me(DetK, ReqFrm, {What,?FUNCTION}, Reply),
    Ret.
    
add_reactors(KSpecPairs, ReqFrm, DetK, D0) when is_list(KSpecPairs) ->
    ?trace_me(ReqFrm, DetK, ?FUNCTION, KSpecPairs),
    {OkL,ErrL,LastD} =
        lists:foldl(
            fun
                ({K,Spec}, {OkAcc,ErrAcc,D1}) ->
                    case add_reactor({K,Spec}, DetK, DetK, D1) of
                        {{ok,_}=Ok, D2} ->
                            {[Ok|OkAcc], ErrAcc, D2};
                        {{error,_}=Err, D2} ->
                            {OkAcc, [Err|ErrAcc], D2}
                    end;
                (Invalid, {OkAcc,ErrAcc,D1}) ->
                    Err = {error, {?FUNCTION,"Expecting a tuple 2.",Invalid}},
                    {OkAcc, [Err|ErrAcc], D1}
            end, {[],[],D0}, KSpecPairs),
    Reply = {What,_} = 
        case ErrL of
            []  -> {ok, lists:reverse(OkL)};
            _   -> {error, {?FUNCTION,"Failed to add one or more reactors.",
                    [{failures,lists:reverse(ErrL)},
                     {successes,lists:reverse(OkL)}]}}
        end,
    ?trace_me(DetK, ReqFrm, {What,?FUNCTION}, Reply),
    {Reply, LastD};
add_reactors(Invalid, ReqFrm, DetK, D) ->
    ?trace_me(ReqFrm, DetK, ?FUNCTION, Invalid),
    Reply = {error, {?FUNCTION,"Expecting a property list.", Invalid}},
    ?trace_me(DetK, ReqFrm, {error,?FUNCTION}, Reply),
    {Reply, D}.

add_reactors_atomic(KSpecPairs, ReqFrm, DetK, D0) ->
    ?trace_me(ReqFrm, DetK, ?FUNCTION, KSpecPairs),
    Ret = {{What,_}=Reply, _} =
        case add_reactors(KSpecPairs, DetK, DetK, D0) of
            {{ok,_}, _}=AllSuccess ->
                AllSuccess;
            {{error, {_,_,[{failures,ErrL},_]}}, _} ->
                ?trace_me(DetK, DetK, {'rollback',?FUNCTION}, KSpecPairs),
                Err1 = {error, {?FUNCTION,
                                "No reactors added - there were errors.",
                                ErrL}},
                {Err1, D0}
        end,
    ?trace_me(DetK, ReqFrm, {What,?FUNCTION}, Reply),
    Ret.

remove_reactor(K, ReqFrm, DetK, D0) ->
    ?trace_me(ReqFrm, DetK, ?FUNCTION, K),
    Ret = {{What,_}=Reply, _} =
        case dict:find(K, D0) of
            error ->
                {{error,{not_found,K}}, D0};
            {ok, #reactor{tripped_cb=TrippedCB,reset_cb=ResetCB}} ->
                {{ok,{K,{TrippedCB,ResetCB}}}, dict:erase(K, D0)}
        end,
    ?trace_me(DetK, ReqFrm, {What,?FUNCTION}, Reply),
    Ret.
    
remove_reactors(KL, ReqFrm, DetK, D0) when is_list(KL) ->
    ?trace_me(ReqFrm, DetK, ?FUNCTION, KL),
    {OkL,ErrL,LastD} =
        lists:foldl(
            fun (K, {OkL,ErrAcc,D1}) ->
                case remove_reactor(K, DetK, DetK, D1) of
                    {{ok,_}=Ok, D2} ->
                        {[Ok|OkL], ErrAcc, D2};
                    {{error,_}=Err, D2} ->
                        {OkL, [Err|ErrAcc], D2}
                end
            end, {[],[],D0}, KL),
    Reply = {What,_} =
        case ErrL of
            []  -> {ok, lists:reverse(OkL)};
            _   -> {error, {?FUNCTION,"Failed to remove one or more reactors.",
                            [{failures,lists:reverse(ErrL)},
                            {successes,lists:reverse(OkL)}]}}
        end,
    ?trace_me(DetK, ReqFrm, {What,?FUNCTION}, Reply),
    {Reply, LastD};
remove_reactors(Invalid, ReqFrm, DetK, D) ->
    ?trace_me(ReqFrm, DetK, ?FUNCTION, Invalid),
    Reply = {error, {?FUNCTION,"Expecting a key list.",Invalid}},
    ?trace_me(DetK, ReqFrm, {error,?FUNCTION}, Reply),
    {Reply, D}.

remove_reactors_atomic(KL, ReqFrm, DetK, D0) ->
    ?trace_me(ReqFrm, DetK, ?FUNCTION),
    Ret = {{What,_}=Reply, _} =
        case remove_reactors(KL, DetK, DetK, D0) of
            {{ok,_}, _}=AllSuccess ->
                AllSuccess;
            {{error, {_,_,[{failures,ErrL},_]}}, _} ->
                ?trace_me(DetK, DetK, {'rollback',?FUNCTION}, KL),
                Err1 = {error, {?FUNCTION,
                                "No reactors removed - there were errors.",
                                ErrL}},
                {Err1, D0}
        end,
    ?trace_me(DetK, ReqFrm, {What,?FUNCTION}, Reply),
    Ret.
    
remove_all_reactors(ReqFrm, DetK, D0) ->
    ?trace_me(ReqFrm, DetK, ?FUNCTION),
    Ret = {{ok,_}=Reply, _} = % This should never fail
            remove_reactors(dict:fetch_keys(D0), DetK, DetK, D0),
    ?trace_me(DetK, ReqFrm, {ok,?FUNCTION}, Reply),
    Ret.

which_reactors(ReqFrm, DetK, D) ->
    ?trace_me(ReqFrm, DetK, ?FUNCTION),
    Reply = {ok, dict:to_list(D)},
    ?trace_me(DetK, ReqFrm, {ok,?FUNCTION}, Reply),
    {Reply, D}.

call_all_reactors({DetS,ESPRes,{MaxR,MaxTu}=FautlyFreq,ReplayCont},
                  ReqFrm, DetK, D0)
    when is_number(MaxR), is_number(MaxTu)
    ->
    ?trace_me(ReqFrm, DetK, ?FUNCTION, {DetS,ESPRes}),
    {ErrL,LastD} =
        dict:fold(
            fun (K, Rtor, {ErrAcc,D1}) ->
                case call_reactor({K,DetS,ESPRes,FautlyFreq,ReplayCont},
                                  DetK, DetK, Rtor, D1)
                of
                    {do_nothing, D2} ->
                        {ErrAcc, D2};
                    {ok, D2} ->
                        {ErrAcc, D2};
                    {{error,_}=Err, D2} ->
                        {[Err|ErrAcc], D2}
                end
            end, {[],D0}, D0),
    Reply = {What,_} =
        case ErrL of
            [] -> {ok, all};
            _  -> {error, {?FUNCTION,"Failed to call one or more reactors.",
                           [{failures,lists:reverse(ErrL)}]}}
        end,
    ?trace_me(DetK, ReqFrm, {What,?FUNCTION}, Reply),
    {Reply, LastD};
call_all_reactors(Invalid, ReqFrm, DetK, D) ->
    ?trace_me(ReqFrm, DetK, ?FUNCTION, Invalid),
    Reply = {error, {?FUNCTION,"Expecting: state(), any(), {number(),number()}",
                     Invalid}},
    ?trace_me(DetK, ReqFrm, {error,?FUNCTION}, Reply),
    {Reply, D}.

call_reactor({K,DetS,ESPRes,FautlyFreq,ReplayCont}, ReqFrm, DetK, Rtor0, D0) ->
    ?trace_me(ReqFrm, DetK, ?FUNCTION, {K,DetS}),
    CBTup0 = {_, CB} =
        case DetS of
            tripped -> {tripped_cb, Rtor0#reactor.tripped_cb};
            reset   -> {reset_cb, Rtor0#reactor.reset_cb}
        end,
    Ret = {Reply, _} =
        case CB=/=do_nothing
             andalso
             do_cb(K, ESPRes, CBTup0, ReplayCont, DetK)
        of
            false ->
                {What = do_nothing, D0};
            ok ->
                {What = ok, D0};
            {error=What,_}=Err ->
                ?warning('detector', "Detector ~p: reactor ~p ~p callback "
                                     "caused an error.", [DetK, K, DetS], Err),
                case do_bump_err_cnt(Rtor0, FautlyFreq) of
                    {ok, Rtor1} ->
                        {Err, dict:store(K, Rtor1, D0)}; 
                    {faulty, Rtor1} ->
                        _ = ?report_me(ReplayCont, DetK, K, "Faulty Reactor",
                                       {K,Rtor1}),
                        ?warning('detector',
                                 "Detector ~p: Removing faulty reactor ~p - "
                                 "too many frequent errors.", [DetK, K], Rtor1),
                        {{ok,_},D1} = remove_reactor(K, DetK, DetK, D0),
                        {Err, D1}
                end
        end,
    ?trace_me(DetK, ReqFrm, {What,?FUNCTION}, [{K,DetS}, {'reply',Reply}]),
    Ret.

do_cb(K, ESPRes, {CBTag,#callback{'fun'=F,args=Args,fun_info=FInfo}},
      ReplayCont, DetK)
      when not is_pid(ReplayCont) % ASSERT table handle|undefined: Passing a collector pid would also work but would be inefficient.
    ->                          
    _ = ?report_me(ReplayCont, DetK, K, CBTag,
                   [F, {'esp_res',ESPRes}, {'args',Args}, {'fun_info',FInfo}]),
    ReactorId = {DetK,self(),K},
    Ret =
        case
            try F(ReactorId, ESPRes, Args)
            catch _:Why ->
                Code =  case CBTag of
                            tripped_cb -> reactor_tripped_cb_fun;
                            reset_cb -> reactor_reset_cb_fun
                        end,
                Who = [{culprit, {ReactorId,{F,[ESPRes,Args],FInfo}}},
                       {stacktrace, erlang:get_stacktrace()}],
                {error, {Code,Why,Who}}
            end
        of
            ok=What->
                ok;
            {error=What, {reactor_tripped_cb_fun,_,_}}=Caught ->
                Caught;
            {error=What, {reactor_reset_cb_fun,_,_}}=Caught ->
                Caught;
            BadRet ->
                Who1 = {culprit, {ReactorId,{F,[ESPRes,Args],FInfo}}},
                Code1 = case CBTag of
                            tripped_cb -> reactor_tripped_cb_return;
                            reset_cb -> reactor_reset_cb_return
                        end,
                {What = error, {Code1,BadRet,Who1}}
       end,
    _ = ?report_me(ReplayCont, K, DetK, {What,CBTag}, Ret),
    Ret.

do_bump_err_cnt(Rtor0, {FautlyMaxR,FaultyMaxTu}) ->
    Cnt1 = Rtor0#reactor.cb_err_cnt + 1,
    if
        Cnt1 =< FautlyMaxR; FautlyMaxR=:=0; FaultyMaxTu=:=0 ->
            {ok, Rtor0#reactor{cb_err_cnt=Cnt1}};
        true ->
            TS = erlang:now(),
            FirstTS1 =  case Rtor0#reactor.first_cb_err_ts of 
                            undefined -> TS;
                            Prev -> Prev
                        end,
            What =  case timer:now_diff(TS, FirstTS1) =< FaultyMaxTu of
                        true -> ok;
                        false -> faulty
                    end,
            {What, Rtor0#reactor{first_cb_err_ts=FirstTS1,cb_err_cnt=Cnt1}}
    end.
    
%%% Helpers --------------------------------------------------------------------

make_reactor([TrippedSpec, ResetSpec]) ->
    #reactor{tripped_cb = make_cb(TrippedSpec, handle_tripped),
             reset_cb = make_cb(ResetSpec, handle_reset)};
make_reactor({M, TrippedArgs, ResetArgs})  ->
    #reactor{tripped_cb = make_cb({M,handle_tripped,TrippedArgs}, undefined),
             reset_cb = make_cb({M,handle_reset,ResetArgs}, undefined)};
make_reactor(TrippedSpec) ->
    #reactor{tripped_cb = make_cb(TrippedSpec, handle_tripped),
             reset_cb = do_nothing}.
             
make_cb(do_nothing, _DfltFAtom) ->
    do_nothing;
make_cb({M,FAtom,Args}, _) ->
    F = erlang:make_fun(M, FAtom, 3),
    #callback{'fun' = F, args = Args, fun_info = erlang:fun_info(F)};
make_cb({F,Args}, _) when is_function(F, 3) ->
    #callback{'fun' = F, args = Args, fun_info = erlang:fun_info(F)};
make_cb({M,Args}, DfltFAtom) ->
    F = erlang:make_fun(M, DfltFAtom, 3),
    #callback{'fun' = F, args = Args, fun_info = erlang:fun_info(F)};
make_cb(Invalid, _) ->
    throw({badarg,Invalid}).
    
    