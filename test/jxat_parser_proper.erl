-module(jxat_parser_proper).

-export([to_string/1, to_binary/1]).

-include_lib("proper/include/proper.hrl").

to_string({ident, Ident, _}) ->
    Ident;
to_string({char, Char, _}) ->
    [$\\, Char];
to_string({syntax_quote, Ast, _}) ->
    "`" ++ to_string(Ast);
to_string({unquote_splicing, Ast, _}) ->
    "~@" ++ to_string(Ast);
to_string({unquote, Ast, _}) ->
    "~" ++ to_string(Ast);
to_string({quote, Ast, _}) ->
    "'" ++ to_string(Ast);
to_string({integer, I, _}) ->
    erlang:integer_to_list(I);
to_string({float, F, _}) ->
    erlang:float_to_list(F);
to_string({string, S, _}) ->
    "\"" ++ S ++ "\"";
to_string({vector, Items, _}) ->
    lists:flatten(["[", lists:map(fun(Item) ->
                                          [" ", to_string(Item), " "]
                                  end, Items), "]"]);
to_string({list, Items, _}) ->
    lists:flatten(["(", lists:map(fun(Item) ->
                                          [" ", to_string(Item), " "]
                                  end, Items), ")"]).

to_binary(AST) ->
   erlang:list_to_binary(to_string(AST)).

compare({ident, Ident, _}, {ident, Ident, _}) ->
    true;
compare({char, Char, _}, {char, Char, _}) ->
    true;
compare({syntax_quote, A1, _}, {syntax_quote, A2, _}) ->
    compare(A1, A2);
compare({unquote_splicing, A1, _}, {unquote_splicing, A2, _}) ->
    compare(A1, A2);
compare({unquote, A1, _}, {unquote, A2, _}) ->
    compare(A1, A2);
compare({quote, A1, _}, {quote, A2, _}) ->
    compare(A1, A2);
compare({integer, I, _}, {integer, I, _}) ->
    true;
compare({float, F, _}, {float, F, _}) ->
    true;
compare({string, S, _}, {string, S, _}) ->
    true;
compare({vector, V1, _}, {vector, V2, _}) ->
    lists:all(fun({I1, I2}) ->
                      compare(I1, I2)
              end, lists:zip(V1, V2));
compare({list, V1, _}, {list, V2, _}) ->
    lists:all(fun({I1, I2}) ->
                      compare(I1, I2)
              end, lists:zip(V1, V2)).

%%------------------------------------------------------------------------------
%% Properties
%%------------------------------------------------------------------------------

prop_parser() ->
    ?FORALL({Expr}, {expression()},
            begin
                BinExpr = to_binary(Expr),
                {ParsedExpr, _, _} = jxa_parser:parse(BinExpr),
                compare(Expr, ParsedExpr)
            end).

%%-----------------------------------------------------------------------------
%% Generators
%%-----------------------------------------------------------------------------
string_character() ->
    union([integer(0, 33),
           integer(35, 91),
           integer(93, 127)]).

internal_string() ->
    ?LET(S, list([string_character()]),
         erlang:binary_to_list(unicode:characters_to_binary(S))).

ident_initial() ->
    union([33,
           integer(35, 38),
           integer(42, 47),
           integer(58, 90),
           integer(94, 95),
           integer(97, 125)]).

ident_character() ->
    union([33,
           integer(35, 38),
           integer(42, 47),
           integer(58, 90),
           integer(94, 125)]).

ident_string() ->
    ?LET({S1, S2},
         {ident_initial(),
          list([ident_character()])},
         [S1 | erlang:binary_to_list(unicode:characters_to_binary(S2))]).

keyword_style_ident() ->
    ?LET(S, ident_string(),
         ":" ++ S).

defvar_style_ident() ->
    ?LET(S, ident_string(),
         "*" ++ S ++ "*").
split_ident() ->
    ?LET({S1, S2}, {ident_string(), ident_string()},
         S1 ++ "-" ++ S2).

normal_ident() ->
    ident_string().

ident() ->
    {ident, union([normal_ident(),
                   split_ident(),
                   defvar_style_ident(),
                   keyword_style_ident()]), 0}.

character() ->
    ?LET(Char, string_character(),
         {char, list_to_binary([Char]), 0}).

syntax_quote(0) ->
    {syntax_quote, {list, [], 0}, 0};
syntax_quote(Size) ->
    {syntax_quote, value(Size div 4), 0}.

unquote_splicing(0) ->
    {unquote_splicing, {list, [], 0}, 0};
unquote_splicing(Size) ->
    {unquote_splicing, jxa_list(Size div 4), 0}.

unquote(0) ->
    {unquote, {list, [], 0}, 0};
unquote(Size) ->
    {unquote, value(Size div 4), 0}.

quote(0) ->
    {quote, {list, [], 0}, 0};
quote(Size) ->
    {quote, value(Size div 4), 0}.

jxa_int() ->
    {integer, integer(), 0}.

jxa_float() ->
    {float, float(), 0}.

jxa_string() ->
    {string, internal_string(), 0}.

jxa_vector(0) ->
    {vector, [], 0};
jxa_vector(Size) ->
    {vector, resize(Size, list(value(Size - 1))), 0}.

jxa_list(0) ->
    {list, [], 0};
jxa_list(Size) ->
    {list, resize(Size, list(value(Size div 4))), 0}.

value(Size) ->
    union([jxa_vector(Size),
           jxa_list(Size),
           jxa_float(),
           jxa_int(),
           jxa_string(),
           quote(Size),
           unquote(Size),
           unquote_splicing(Size),
           syntax_quote(Size),
           character(),
           ident()]).

expression() ->
    %%?SIZED(N, value(N)).
    value(10).
