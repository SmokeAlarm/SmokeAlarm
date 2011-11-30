%%%-----------------------------------------------------------------------------
%%%
%%% Desc:       An Event Stream Processor
%%%
%%%             Based on code from the Buyaka ! Event Processor with permission.
%%%
%%% Purpose:    - Detect a complex, contigious or non-contigious, ordered or
%%%               unordered series of events out of a stream of events according
%%%               to a detection specification.
%%%             - Optionally accumlate data (as per detection specification)
%%%               that will eventually be passed on to reactors.
%%%
%%%-----------------------------------------------------------------------------

-module(sa_esp).

-legal("Copyright (c) 2011, The Authors.\n"
        "http://github.com/SmokeAlarm\n"
        "See LICENSE.txt for licensing information.\n"
        "**\n"
        "This file is based on code from the Buyaka ! Event Processor with "
        "permission from Hysteria Technological Ltd.\n"
        "http://www.hysteria-tech.com\n"
        "**").
-author("Edmond Begumisa <ebegumisa at hysteria dash tech dot com>").

%% Public Interface
-export([new/1, is_esp/1, eval_event/4]).

%% Private Interface
-export([]).

%% Private Preprocess
-include("common.hrl").

-record(tree, {     % State of a match clause
            match_fun,      % - MatchFun of a statement's {match, Fun} clause
            match_hits,     % - If there are child clauses -- Sliding window:
                            %       undefined|stdlib:queue of #match_hit{} recs
                            %       for events satisfying this match clause AND
                            %       are still within children limits.
                            %   If no child clauses -- undefined|#match_hit{} 
                            %       for event satisfying match clause.
            e_cnt_sum=0,    % - If there are child clauses -- the total of all
                            %   match_hits #match_hit{}.e_cnt
            children=[]     % - #child{} records list of and_within|or_within
                            %   clauses corresponding to this match clause  
        }).             
             
-record(child, {    % State of an and_within|or_within clause
            con,        % - Condition (and_within|or_within)
            period,     % - Required max number of events or amount of time
                        %   integer()|float()
            unit,       % - Unit for period -- Atom: events|microseconds||
                        %   milliseconds|seconds|minutes|hours
            trg_min,    % - Required minimum number of triggers for this
                        %   child clause
            trg_cnt_sum=0,
            tree        % State of this child clause's inner match clause and
                        %   it's children (#tree{} record)
        }).         
             
-record(match_hit, {    % Information about an event that satisfied a match clause
            ts,             % - Timestamp of the matched event -- same format as 
                            %   erlang:now() i.e {MegaSecs,Secs,MicroSecs} 
            e_cnt=0,        % - Event count since the matched event (i.e. number
                            %   of OTHER events that have occured since the matched
                            %   event)
            trg_cnts,       % - Current count of detections for child clauses
            acc,            % - User Data returned from MatchFun i.e. Second
                            %   element of a...
                            %       {true,*Acc*} -- non leaf match fun return or
                            %       {true,*Acc*,Res} -- leaf match fun return
            res             % - Result from a leaf MatchFun i.e thrid element of
                            %   above.
        }).
        
%%% Interface ------------------------------------------------------------------

new(DetSpec) ->
    new_tree(DetSpec, DetSpec).

is_esp(S) when is_record(S, tree) ->
    true;
is_esp(_) ->
    false.

%% Take a detector statement spec, check it for validity (defensively failing
%% early with useful errors) then convert it to a new statement state tree.
new_tree([], _) ->
    #tree{};
new_tree([{match, F}=H|T], OrigSpec) when is_function(F) ->
    case erlang:fun_info(F, arity) of
        {arity, 3} ->
            #tree{match_fun = F, children = new_child(T, [], true, OrigSpec)};
        {arity, N} ->
            ?error('invalid_esp_statement',
                   "Wrong arity ~w: a 'match' clause parameter must be a fun "
                   "of arity 3.", [N],
                    [{'bad_clause', H}, {fun_info, erlang:fun_info(F)},
                     {'det_spec', OrigSpec}])
    end;
new_tree([{match, Bad}=H|_], OrigSpec) ->
    ?error('invalid_esp_statement',
           "Bad type ~w: a 'match' clause parameter must be a fun.",
           [sa_debug:data_type(Bad)],
           [{'bad_clause', H}, {'det_spec', OrigSpec}]);   
new_tree([H|_], OrigSpec) ->
    ?error('invalid_esp_statement',
           "Unexpected clause: a detection statement/sub-statement must begin "
           "with a 'match' clause.", [], [{'bad_clause', H},
                                          {'det_spec', OrigSpec}]);
new_tree(Statement, OrigSpec) ->
    ?error('invalid_esp_statement',
            "Bad type ~w: a detection statement/sub-statement must be a list.",
            [sa_debug:data_type(Statement)],
            [{'bad_statement', Statement}, {'det_spec', OrigSpec}]).
    
new_child([], ChildrenA0, _, _) ->
    lists:reverse(ChildrenA0);
new_child([{Con, Period, Unit, ChildSpec}|T], ChildrenA0, IsFirst, OrigSpec) ->
    new_child(
        [{Con, Period, Unit, 1, ChildSpec}|T], ChildrenA0, IsFirst, OrigSpec); 
new_child([{or_within, _, _, _, _}=H|_], _, true, OrigSpec) ->
    ?error('invalid_esp_statement',
           "Unexpected clause: an 'or_within' clause cannot come after a "
           "'match' clause, only an 'and_within' clause may.", [],
           [{'bad_clause', H}, {'det_spec', OrigSpec}]);
new_child([{Con, Period, Unit, TrgMin, ChildSpec}=H|T], ChildrenA0, _, OrigSpec)
          when Con=:=and_within; Con=:=or_within
    ->
    try
        true = (is_number(Period) andalso Period > 0),
        true = (is_integer(TrgMin) andalso TrgMin >= 1),
        true = (Unit=:=events orelse Unit=:=microseconds orelse
                Unit=:=milliseconds orelse Unit=:=seconds orelse
                Unit=:=minutes orelse Unit=:=hours),
        true = is_list(ChildSpec)
    catch
        error:{badmatch,false} ->
            ?error('invalid_esp_statement',
                    "Bad type or value: an '~w' clause contains an invalid "
                    "parameter.", [Con],
                    [{'bad_clause', H}, {'det_spec', OrigSpec}])
    end,
    new_child(T, [#child{con = Con, period = Period, unit = Unit,
                         trg_min = TrgMin, tree = new_tree(ChildSpec, OrigSpec)}
                    | ChildrenA0], false, OrigSpec);
new_child([{match, _}=H|_], _, _, OrigSpec) ->
    ?error('invalid_esp_statement',
            "Unexpected clause: a statement/sub-statement cannot contain more "
            "than one 'match' clause", [],
            [{'bad_clause', H}, {'det_spec', OrigSpec}]);
new_child([H|_], _, _, OrigSpec) ->
    ?error('invalid_esp_statement',
            "Unrecognised clause: expecting an 'and_within' or an 'or_within' "
            "clause -- check condition and parameter number.", [],
            [{'bad_clause', H}, {'det_spec', OrigSpec}]).

%% Traverse a detector statement state tree checking for matches against a 
%% specified event, updating tree as we go...
eval_event(Event, {_Nde,_Pid,_TS}=Origin, Acc, #tree{}=State) ->
    eval_match(Event, Origin, State, _IsRoot = true, {ret,undefined,Acc}).

%% * Process an empty statement
eval_match(_, {_,_,{_,_,_}}, #tree{match_fun=undefined, children=[]}=S, _,
          {ret,_,Acc})
    ->
    {cont, Acc, S};
%% * Process a {match,F} clause
eval_match(E, {_,_,{_,_,_}=TS}=O, #tree{match_fun=MF, match_hits=MHits0,
                                        children=Children}=S0, IsRoot,
           {ret,V0,Acc0})
    ->
    case {MF(E, O, Acc0), Children} of
        {false, []} when IsRoot; MHits0=:=undefined ->
            {cont, Acc0, S0};
        {false, []} ->
            #match_hit{e_cnt=MEC0, acc=MAcc, res=MRes}=MHits0,
            MHits1 = MHits0#match_hit{e_cnt = MEC0+1},
            {{earlier_trigger,MRes}, MAcc, S0#tree{match_hits=MHits1}};
        {false, _} ->
            case MHits0=:=undefined orelse queue:is_empty(MHits0) of
                true ->
                    {cont, Acc0, S0};
                false ->
                    S1 = S0#tree{match_hits = bump_newest_hit_e_cnt(MHits0),
                                 e_cnt_sum = S0#tree.e_cnt_sum+1},
                    eval_children(E, O, S1, {ret,V0,Acc0})
            end;
        {{true, Acc1}, [_|_]} ->
            NewMHit = #match_hit{ts = TS, acc = Acc1,
                                 trg_cnts = [0 || _ <- Children]},
            {cont, Acc0, add_new_hit(NewMHit, S0)};
        {{trigger, Acc1, Res1}, []} ->
            NewMHit = #match_hit{ts = TS, acc = Acc1, res = Res1},
            {{trigger,Res1}, Acc1, S0#tree{match_hits = NewMHit}};
        {{trigger, Acc1, Res1}, _} ->
            NewMHit = #match_hit{ts = TS, acc = Acc1, res = Res1,
                                 trg_cnts = [0 || _ <- Children]},
            {{trigger,Res1}, Acc1, add_new_hit(NewMHit, S0)};
        {Bad, _} -> 
            ?error('match_return',
                    common_match_badret_desc(Bad, Children=:=[]),
                    [sa_debug:data_type(Bad)],
                    [{'event', E}, {'origin',O}, {'bad_return', Bad},
                     {'match_fun_info', erlang:fun_info(MF)},
                     {if IsRoot -> 'tree'; true -> 'subtree' end, S0}])
    end.
%% * Process {and_within,..}|{or_within,..} clauses (and thier child statement
%%%  recursivley)
eval_children(E, O, #tree{match_hits=MHits}=S0, {ret,V0,Acc0}) ->
    NewestMTrgCs = (queue:get_r(MHits))#match_hit.trg_cnts,
    {V1, Acc1, S1, WIdx} = do_eval_children(E, O, S0, NewestMTrgCs,
                                            {accs, [], []},
                                            {ret, V0, Acc0},
                                            {oldest_hit_within, 0, undefined}),
    case WIdx of
        0 when V1=/=cont ->
            ?error("Assertion failed");
        0 ->
            {cont, Acc0, S1#tree{match_hits = undefined, e_cnt_sum = 0}};
        _ ->
            PurgeIdx =  case V0 of
                            undefined -> WIdx;
                            cont -> WIdx;
                            _ -> WIdx-1
                        end,
            {V1, Acc1, drop_hit_idx_and_older(PurgeIdx, S1)}
    end.
    
do_eval_children(_, {_,_,{_,_,_}},
                 #tree{children=[],match_hits=MHits0}=S0, _MTrgCs=[],
                 {accs, ChildrenA, MTrgCsA},
                 {ret, V, Acc},
                 {oldest_hit_within, WIdx, _})
    ->
    {{value, #match_hit{}=NewestHit0}, Rest} = queue:out_r(MHits0),
    NewestHit1 = NewestHit0#match_hit{trg_cnts = lists:reverse(MTrgCsA)},
    {V, Acc, S0#tree{children = lists:reverse(ChildrenA),
                     match_hits = queue:in(NewestHit1, Rest)}, WIdx};
do_eval_children(E, {_,_,{_,_,_}=TS}=O,
                 #tree{children=[HChild0|TChildren],
                       match_hits=MHits,
                       e_cnt_sum=MECSum}=S0, [HMTrgC0|TMTrgCs],
                 {accs, ChildrenA0, MTrgCsA0},
                 {ret, V0, Acc0},
                 {oldest_hit_within, WIdx0, WHit0})
    ->
    W1 = {oldest_hit_within,WIdx1,WHit1} =
        case find_older_hit_within_period(
                WIdx0, HChild0#child.period, HChild0#child.unit, TS, MECSum,
                MHits)
        of
            not_found -> {oldest_hit_within,WIdx0,WHit0};
            {ok, OlderIdx, OlderHit} -> {oldest_hit_within,OlderIdx,OlderHit}
        end,
    case WIdx1 of
        0 ->
            HChild1 = HChild0#child{tree = clear_tree(HChild0#child.tree),
                                    trg_cnt_sum = 0},
            HMTrgC1 = 0,
            Ret1 = {ret, cont, Acc0};
        _ ->
            #child{con=Con,trg_min=TrgMin,trg_cnt_sum=TrgCSum0,tree=ChildS0}
                = HChild0,
            {ChildV, ChildAcc, ChildS1} =
                eval_match(E, O, ChildS0, cont, {ret,V0,WHit1#match_hit.acc}),
            HChild1 =
                HChild0#child{
                    tree =  case V0 of
                                undefined -> ChildS1;
                                cont -> ChildS1;
                                {trigger,_} ->
                                    ChildS1#tree{match_hits=undefined}
                            end,
                    trg_cnt_sum = TrgCSum1 =
                            case ChildV of
                                cont -> TrgCSum0;
                                {earlier_trigger,_} -> TrgCSum0;
                                {trigger,_} -> TrgCSum0+1
                            end},
            HMTrgC1 =   case ChildV of
                            cont -> HMTrgC0;
                            {earlier_trigger,_} -> HMTrgC0;
                            {trigger,_} -> HMTrgC0+1
                        end,
            Ret1 =  case Con of
                        and_within when V0=:=cont ->
                            {ret, cont, Acc0};
                        or_within when V0=/=cont, V0=/=undefined ->
                            {ret, cont, Acc0};
                        _ ->
                            V1 = ChildV=/=cont andalso TrgCSum1>=TrgMin,
                            {ret, V1, ChildAcc}
                    end
    end,
    do_eval_children(E, O, S0#tree{children=TChildren}, TMTrgCs,
                     {accs, [HChild1|ChildrenA0], [HMTrgC1|MTrgCsA0]},
                     Ret1, W1).

clear_tree(#tree{children=[]}=S0) ->
    S0#tree{match_hits=undefined,e_cnt_sum=0};
clear_tree(#tree{}=S0) ->
    clear_children(S0#tree{match_hits=undefined,e_cnt_sum=0}, []).
    
clear_children(#tree{children=[]}=S0, ChildrenA0) ->
    S0#tree{children = lists:reverse(ChildrenA0)};
clear_children(#tree{children=[H0|T]}=S0, ChildrenA0) ->
    ChildS1 = clear_tree(H0#child.tree),
    H1 = H0#child{tree = ChildS1, trg_cnt_sum = 0},
    clear_children(S0#tree{children = T}, [H1|ChildrenA0]).
    
%%% Helpers --------------------------------------------------------------------

bump_newest_hit_e_cnt(MHits0) ->
    {{value, #match_hit{e_cnt=EC0}=NewestHit0}, Rest} = queue:out_r(MHits0),
    queue:in(NewestHit0#match_hit{e_cnt = EC0+1}, Rest).

add_new_hit(#match_hit{}=MHit, #tree{match_hits=MHits0,children=Children0}=S0)
    ->
    case MHits0=:=undefined orelse queue:is_empty(MHits0) of
        true ->
            S0#tree{match_hits = queue:in(MHit, queue:new())};
        false ->
            MHits1 = queue:in(MHit, bump_newest_hit_e_cnt(MHits0)),
            MECSum = S0#tree.e_cnt_sum+1,
            S1 = S0#tree{match_hits = MHits1, e_cnt_sum = MECSum},
            case Children0 of
                [] ->
                    S1;
                _ ->
                    OldestHitIdxWithin = 
                        sa_utils:lists_foldl_while(
                            fun(#child{period=Period,unit=Unit}, WIdx0) ->
                                case find_older_hit_within_period(
                                        WIdx0, Period, Unit, MHit#match_hit.ts,
                                        MECSum, MHits1)
                                of
                                    not_found -> WIdx0;
                                    {ok, OlderIdx,_} -> OlderIdx
                                end
                            end, 1, Children0),
                    if
                        OldestHitIdxWithin == 1 ->
                            S1;
                        OldestHitIdxWithin > 1 ->
                            drop_hit_idx_and_older(OldestHitIdxWithin, S1)
                    end     
            end     
    end.
                                
find_older_hit_within_period(OlderThanHitIdx, Period, Unit, TS, MECSum, MHits)
    ->
    WhileWithinF =
        case Unit of
            events ->
                fun
                    (#match_hit{e_cnt=MEC}=MHit, {_, N, MHit, FrontMEC}) when
                    OlderThanHitIdx=:=0; N+1<OlderThanHitIdx
                    ->
                        if
                            MECSum - FrontMEC - 1 =< Period ->
                                {true, {ok, N+1, MHit, FrontMEC+MEC}};
                            true ->
                                false
                        end;
                    (_,_) ->
                        false
                end;
            _ ->
                TSU = ts_to_tunit(TS, Unit),
                fun
                    (#match_hit{ts=MTS}=MHit, {_, N, MHit, 0}) when
                    OlderThanHitIdx=:=0; N+1<OlderThanHitIdx
                    ->
                        case (TSU - ts_to_tunit(MTS, Unit)) of
                            Diff when Diff =< Period  ->
                                {true, {ok, N+1, MHit, 0}};
                            _ ->
                                false
                        end;
                    (_,_) ->
                        false
                end
        end,
    case sa_utils:queue_fold_while(WhileWithinF, {not_found,0,null,0}, MHits) of
        {not_found, 0, null, 0} ->
            not_found;
        {ok, OlderMHitIdx, OlderMHit, _} ->
            {ok, OlderMHitIdx, OlderMHit}
    end.
        
drop_hit_idx_and_older(MHitIdx, {match_hits=MHits0,children=Children0}=S0) ->
    {MHits1, {_,DroppedEC,DroppedTrgCs}} =
        sa_utils:queue_drop_while_folding(
            fun
                (#match_hit{e_cnt=MEC,trg_cnts=MTrgCs},
                 {N,DroppedEC0,DroppedTrgCs0}) when N=<MHitIdx
                ->
                    DroppedTrgCs1 = lists:zipwith(fun(X,Y) -> X+Y end,
                                                  DroppedTrgCs0, MTrgCs),
                    {true, {N+1, DroppedEC0+MEC, DroppedTrgCs1}};
                (_, _) ->
                    false
            end, {1,0,[0 || _ <- Children0]}, MHits0),
    Children1 = lists:zipwith(
                    fun (#child{trg_cnt_sum=TrgCSum0}=Child0, DroppedTrgC) ->
                        Child0#child{trg_cnt_sum = TrgCSum0-DroppedTrgC}
                    end, Children0, DroppedTrgCs),
    S0#tree{match_hits = MHits1,
            e_cnt_sum = S0#tree.e_cnt_sum - DroppedEC,
            children = Children1}.
            
ts_to_tunit({MegaSecs,Secs,MicroSecs}, microseconds) ->
    (MegaSecs*1000000 + Secs)*1000000 + MicroSecs;
ts_to_tunit({MegaSecs,Secs,MicroSecs}, milliseconds) ->
    (MegaSecs*1000000 + Secs)*1000 + (MicroSecs/1000);
ts_to_tunit({MegaSecs,Secs,MicroSecs}, seconds) ->
    MegaSecs*1000000 + Secs + (MicroSecs/1000000);
ts_to_tunit(TS, minutes) ->
    ts_to_tunit(TS, seconds)/60;
ts_to_tunit(TS, hours) ->
    ts_to_tunit(TS, minutes)/60.
    
%% Try and give better descriptions of common mistakes than just pattern match
%% errors.
common_match_badret_desc({true, _}, _IsLeaf = true) ->
    "Unexpected return: a postive return for a leaf 'match' clause must come "
    "with a terminal result -- i.e. a tuple {'trigger', Acc, Res}. Your fun "
    "has returned a ~w {'true', Acc} instead which may only be returned by a "
    "non-leaf 'match' clause to fall into the next clause.";
common_match_badret_desc({false, _}, _) ->
    "Bad return: a negative return for a 'match' clause must come without an "
    "accumulator -- just the atom 'false'. Your fun has returned a ~w "
    "{'false', Acc} instead. Only positive returns come with an accumulator.";
common_match_badret_desc({false, _, _}, _) ->
    "Bad return: a negative return for a 'match' clause must come without an "
    "accumulator or a terminal result -- just the atom 'false'. Your fun has "
    "returned a ~w {'false', Acc, Res} instead. Only terminal positive returns "
    "can come with an accumulator.";
common_match_badret_desc({trigger, _}, _) ->
    "Bad return: a postive terminal return for a 'match' clause must come with "
    "a result -- a tuple {'trigger', Acc, Res}. Your fun has returned a ~w "
    "{'trigger', Acc} instead.";
common_match_badret_desc(_, true) ->
    "Bad return: a leaf 'match' clause must return either the atom 'false' or "
    "the terminal tuple {'trigger', Acc, Res}. Your fun has returned an "
    "unrecognised ~w.";
common_match_badret_desc(_, false) ->
    "Bad return: a non-leaf 'match' clause must return either the atom "
    "'false', a tuple {'true', Acc} to fall into the nexgt clause, or the "
    "terminal tuple {'trigger', Acc, Res}. Your fun has returned an "
    "unrecogised ~w.".
