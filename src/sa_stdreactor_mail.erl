%%%-----------------------------------------------------------------------------
%%%
%%% Desc:       Predefined SmokeAlarm SASL alarm_handler reactor
%%%             
%%% Purpose:    - Provide an easy way to send e-mail whenever a SmokeAlarm
%%%               detector trips/resets.
%%%             - Include any piped data from other reactors as attachments.
%%%         
%%%             TODO: Options for limiting attachment sizes and queing mail
%%%             in an outbox for a couple of hours if individual sends fails.
%%%
%%%-----------------------------------------------------------------------------

-module(sa_stdreactor_mail).
-legal("Copyright (c) 2011, The Authors.\n"
        "http://github.com/SmokeAlarm\n"
        "See LICENSE.txt for licensing information.").
-author("Edmond Begumisa <ebegumisa at hysteria dash tech dot com>").

%% Public Interface
-export([handle_tripped/4, handle_reset/3]).

%% Private Preprocess
-record(opts, {to, cc, bcc, reply_to, body_desc, smtp_opts}).
-include("sa_utils.hrl").
-import(sa_utils, [to_valid_utf8_bin/1, to_utf8_bin/1,
                   to_valid_utf8_list/1, to_utf8_list/1]).
 
-define(DFLT_BODY_DESC_FMT,
        "-- This is an automatically generated message --~n~n"
        "Don't be alarmed, this has NOTHING TO DO WITH FIRE!~n~n"
        "My name is SmokeAlarm. I am problem-detection software running in an "
        "Erlang Virtual Machine. My job is to keep an eye on one or more "
        "computers running Erlang software for my boss the systems' "
        "administrator.~n~n"
        "You have recieved this message because you have been configured by "
        "my administrator as a reciepient of e-mail alerts when I notice a "
        "change in the ~w software detector.~n~n"
        "Please be advised of the following:").

%%% Interface ------------------------------------------------------------------

handle_tripped(ReactorId, TrippedResult, Opts, PipeAcc) when
               is_list(Opts)
    ->
    {ok,_Pid} =
        send(ReactorId, tripped, TrippedResult, to_opts_rec(Opts), PipeAcc),
    ok.
    
handle_reset(ReactorId, ResetResult, Opts) ->
    {ok,_Pid} =
        send(ReactorId, reset, ResetResult, to_opts_rec(Opts), []),
    ok.

send({DtorKey,_,_}=RtorId, DtorState, Res, #opts{}=Opts, Atchs) ->
    io:format("<<< REPLYTO: ~p~n", [string:join(Opts#opts.reply_to, ", ")]),
    FromName = ["SmokeAlarm", try
                                {ok, Host} = inet:gethostname(),
                                [" running at ", Host]
                              catch _:_ -> ""
                              end],
    FromAddr = ["<smokealarm.noreply.", atom_to_list(node()), ">"],
    Hdrs =  hdrs(DtorKey, DtorState, [FromName,FromAddr], Opts),
    Everyone = [Addr || [_,Addr]
                    <- Opts#opts.to ++ Opts#opts.cc ++ Opts#opts.bcc],
    IsMultiRecip =  case Everyone of [_] -> false; [_|_] -> true end,
    Body = body(RtorId, DtorState, Res, IsMultiRecip, Opts),
    MimeMail = sa3p_gen_smtp_mimemail:encode(
                    case Atchs of
                        [] ->
                            setelement(3, Body, Hdrs);
                        [_|_] ->
                            {<<"multipart">>, <<"mixed">>, Hdrs, [],
                                 [Body | [attach(Atch) || Atch <- Atchs]]}
                    end),
    sa3p_gen_smtp_client:send({iolist_to_binary(FromAddr),Everyone,MimeMail},
                              Opts#opts.smtp_opts).

hdrs(DtorKey, DtorState, [_,_]=FromNameAddr, #opts{}=Opts) ->
    [{<<"To">>, string:join(Opts#opts.to, ", ")},
     {<<"From">>, to_utf8_bin(FromNameAddr)},
     {<<"Reply-To">>,
        case Opts#opts.reply_to of
            [] -> <<"<>">>;
            ReplyTo -> string:join(ReplyTo, ", ")
        end},
     {<<"Subject">>,
        to_utf8_bin(io_lib:format("Detector ~w has ~w", [DtorKey, DtorState]))},
     {<<"X-Mailer">>,
        iolist_to_binary(["SmokeAlarm",
                         case application:get_key(smokealarm, vsn) of
                            {ok, Vsn} -> [" ", Vsn];
                            _ -> ""
                         end,
                         " (http://github.com/SmokeAlarm)"])}
      | case Opts#opts.cc of
           [] -> [];
           [_|_] -> [{<<"Cc">>, string:join(Opts#opts.cc, ", ")}]
        end].
                
body({DtorKey,DtorPid,RtorKey}, DtorState, Res, IsMultiRecip, #opts{}=Opts) ->
    Text = io_lib:format(
            "~ts~n~n"
            "== SmokeAlarm ====~n"
            "Reactor: ~p~n"
            "Detector: ~p ~w~n"
            "Flipped To: ~w~n"
            "Result: ~p~n"
            "====~n~n"
            "~ts"
            "Have a nice day!~n~n"
            "/smokealarm application at node ~w.~n"
            "http://www.github.com/SmokeAlarm~n~n",
            [case Opts#opts.body_desc of
                undefined -> io_lib:format(?DFLT_BODY_DESC_FMT, [DtorKey]);
                CustomDesc -> CustomDesc
             end,
             RtorKey, DtorKey, DtorPid, DtorState, Res, 
             case Opts#opts.reply_to of
                [] when IsMultiRecip ->
                    "Do not include me in your reply. Sorry, I cannot recieve "
                    "e-mail.\r\n\r\n";
                [] ->
                    "Do not reply to me. Sorry, I cannot recieve "
                    "e-mail.\r\n\r\n";
                _ ->
                    ""
             end, node()]),
    {<<"text">>, <<"plain">>, [], [], to_utf8_bin(Text)}.

attach({K,V}) ->
    PrintF = fun(X) ->
                try io_lib:format("~ts", [X])
                catch _:_ -> io_lib:format("~p", [X])
                end
             end,
    Filename = [Chr || Chr <- PrintF(K), Chr=/=$\n, Chr=/=$\r],
    Data = list_to_binary(PrintF(V)),
    Pars = [{<<"transfer-encoding">>, <<"base64">>},
            {<<"disposition">>, <<"attachment">>},
            {<<"disposition-params">>,
                [{<<"filename">>, list_to_binary(Filename)}]}],
    {<<"application">>, <<"octet-stream">>, [], Pars, Data};
                % MAYBE TODO: ^^^ Are mime type lookups worth the effort?
attach(Bad) when is_binary(Bad) ->
    attach({"untitled.dat", Bad});
attach(Bad)  ->
    attach({"untitled.txt", Bad}).
    
to_opts_rec(OptsL0) when is_list(OptsL0)->
    KnownL = ?PROPLIST_FROM_RECORD(#opts{}, opts),
    case sa_utils:proplists_subtract(OptsL0, KnownL) of
        [] ->
            OptsL1 = [chk_opt(X) ||
                        X <- sa_utils:proplists_umerge(OptsL0, KnownL)],
            ?PROPLIST_TO_RECORD(OptsL1, opts);
        Unknown ->
            throw({'unknown_opts', Unknown})
    end.

chk_opt({to, undefined}=Bad) ->
    throw({'bad_opt', Bad});    
chk_opt({to, V}=Bad) when V=:=""; V=:=<<>> ->
    throw({'bad_opt', Bad});    
chk_opt({K, undefined}) when K=:=to; K=:=cc; K=:=bcc; K=:=reply_to ->
    {K, []};
chk_opt({K, V0}=Opt) when K=:=to; K=:=cc; K=:=bcc; K=:=reply_to ->
    DoF =   fun(L) ->
                try {K, parse_name_addr_list(L)}
                catch _:Why -> throw({'bad_opt', {Opt,Why}})
                end
            end, 
    if
        is_binary(V0) -> DoF([V0]);
        is_list(V0) ->
            case to_valid_utf8_bin(V0) of
                {error, Why} -> throw({'bad_opt', {Opt,Why}});
                {ok, _} -> DoF(V0)
            end;        
        true -> throw({'bad_opt', Opt})
    end;
chk_opt({body_desc, undefined}=Opt) ->
    Opt;
chk_opt({body_desc, V}=Opt) when is_binary(V) ->
    Opt;
chk_opt({body_desc, V}=Opt) when is_list(V) ->
    case to_valid_utf8_bin(V) of
        {ok, _} -> Opt;
        {error,Why} -> throw({'bad_opt', {Opt,Why}})
    end;    
chk_opt({smtp_opts, undefined}) ->
    chk_opt({smtp_opts, []});
chk_opt({smtp_opts, V}=Opt) when is_list(V) ->
    case sa3p_gen_smtp_client:check_options(V) of
        ok -> Opt;
        Err -> throw({bad_opt, {Opt,Err}})
    end;
chk_opt(Bad) ->
    throw({'bad_opt', Bad}).

%% RecipOrRecipsL: List of individual recipients OR list of comma-separated
%% recipients OR mix of either.
%%  e.g: [" Name1 <Addr1> ", <<\"Name2\" <Addr2>">>, "<Addr3>", "Addr4"]
%%              ...which is the same as...
%%       [<<" Name1 <Addr1> ,\"Name2\" <Addr2>">>, "<Addr3>, Addr4"]
%%              ...which is the same as...
%%       [" Name1 <Addr1> , \"Name2\" <Addr2>, <Addr3>, Addr4"]
%%  which all return:
%%      [[<<"Name1 ">>,<<"<Addr1>">>], [<<"\"Name2\" ">>,<<"<Addr2>">>],
%%       [<<>>,<<"<Addr3>">>], [<<>>,<<"Addr4">>]]
parse_name_addr_list(RecipOrRecipsL) when is_list(RecipOrRecipsL) ->
    Quoted = "^\\s*(\"[^\"]*\"\\s*)([\\S]*)\\s*$",
    Unquoted = "^\\s*([^<\"]*)(<[^>\"\\s]*>)\\s*$",
    {ok, RE} = re:compile(["(?:", Quoted, ")|(?:", Unquoted, ")"], [unicode]),
    ParseF =
        fun(Recip, Acc0) ->
            [case re:split(Recip, RE) of
                [_,<<>>,<<>>,<<>>,<<>>,_] -> [<<>>, Recip];
                [_,<<>>,Addr,<<>>,<<>>,_] -> [<<>>, Addr];
                [_,<<>>,<<>>,<<>>,Addr,_] -> [<<>>, Addr];
                [_,Name,Addr,<<>>,<<>>,_] when Name=/=<<>>, Addr=/=<<>> ->
                    [Name, Addr];
                [_,<<>>,<<>>,Name,Addr,_] when Name=/=<<>>, Addr=/=<<>> ->
                    [Name, Addr];
                [_,_   ,_   ,_   ,_   ,_] when is_binary(Recip) ->
                    [<<>>, Recip];
                [_,_   ,_   ,_   ,_   ,_] -> [<<>>, to_utf8_bin(Recip)];
                [RecipB]                  -> [<<>>, RecipB]
             end | Acc0]
        end,
    lists:reverse(
        lists:foldl(
            fun(RecipOrRecips0, Acc0) ->
                RecipOrRecips1 =
                    case to_valid_utf8_list(RecipOrRecips0) of
                        {ok, Valid} -> Valid;
                        {error, Why} -> throw({error, {RecipOrRecips0,Why}})
                    end,
                lists:foldl(ParseF, Acc0, string:tokens(RecipOrRecips1, ","))
            end, [], RecipOrRecipsL)).
            