%% -*- mode: Erlang; fill-column: 80; comment-column: 76; -*-
-module(jxa_utils).

-export([to_core/1, to_ast/1, to_asm/1,
         core_from_file/1, ast_from_file/1,
         asm_from_file/1]).

%%=============================================================================
%% Public API
%%=============================================================================
core_from_file(Path) ->
    {ok, Contents} = file:read_file(Path),
    to_core(binary_to_list(Contents)).

ast_from_file(Path) ->
    {ok, Contents} = file:read_file(Path),
    to_ast(binary_to_list(Contents)).

asm_from_file(Path) ->
    {ok, Contents} = file:read_file(Path),
    to_asm(binary_to_list(Contents)).

to_ast(StringExpr) ->
    Forms0 =
        lists:foldl(fun(<<>>, Acc) ->
                            Acc;
                       (<<"\n\n">>, Acc) ->
                            Acc;
                       (El, Acc) ->
                            {ok, Tokens, _} =
                                erl_scan:string(binary_to_list(El)
                                                ++ "."),
                            [Tokens | Acc]
                    end, [], re:split(StringExpr, "\\.\n")),
    %% No need to reverse. This will rereverse for us
    lists:foldl(fun(Form, Forms) ->
                        {ok, ErlAST} = erl_parse:parse_form(Form),
                        [ErlAST | Forms]
                end, [], Forms0).

to_core(StringExpr) ->
    compile:forms(to_ast(StringExpr), [to_core]).

to_asm(StringExpr) ->
    compile:forms(to_ast(StringExpr), ['S']).
