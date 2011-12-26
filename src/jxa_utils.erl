%% -*- mode: Erlang; fill-column: 80; comment-column: 76; -*-
-module(jxa_utils).

-export([to_core/1, from_file/1]).

%%=============================================================================
%% Public API
%%=============================================================================
from_file(Path) ->
    {ok, Contents} = file:read_file(Path),
    to_core(binary_to_list(Contents)).

to_core(StringExpr) ->
    Forms0 =
        lists:foldl(fun(<<"\n">>, Acc) ->
                            Acc;
                       (El, Acc) ->
                            {ok, Tokens, _} = erl_scan:string(binary_to_list(El)
                                                              ++ "."),
                            [Tokens | Acc]
                    end, [], re:split(StringExpr, "\\.")),
    %% No need to reverse. This will rereverse for us
    NForms =
        lists:foldl(fun(Form, Forms) ->
                            {ok, ErlAST} = erl_parse:parse_form(Form),
                            [ErlAST | Forms]
                    end, [], Forms0),
    compile:forms(NForms, [to_core]).
