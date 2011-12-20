-module(jxat_parser_proper).

-export([to_string/1, to_binary/1]).

-include_lib("proper/include/proper.hrl").

to_string({ident, Ident, _}) ->
    atom_to_list(Ident);
to_string({quote, Value, _}) ->
   "'" ++ to_string(Value);
to_string({fun_ref, {Module, Fun, Arity}, _}) ->
    Module ++ ":" ++
        Fun ++ "/" ++
        integer_to_list(Arity);
to_string({fun_ref, {Fun, Arity}, _}) ->
    Fun ++ "/" ++
        integer_to_list(Arity);
to_string({char, Char, _}) ->
    [$\\, Char];
to_string({integer, I, _}) ->
    erlang:integer_to_list(I);
to_string({float, F, _}) ->
    erlang:float_to_list(F);
to_string({string, S, _}) ->
    "\"" ++ S ++ "\"";
to_string({vector, Items, _}) ->
    lists:flatten(["{", lists:map(fun(Item) ->
                                          [" ", to_string(Item), " "]
                                  end, Items), "}"]);
to_string({list, Items, _}) ->
    lists:flatten(["(", lists:map(fun(Item) ->
                                          [" ", to_string(Item), " "]
                                  end, Items), ")"]);
to_string({literal_list, Items, _}) ->
    lists:flatten(["[", lists:map(fun(Item) ->
                                          [" ", to_string(Item), " "]
                                  end, Items), "]"]).

to_binary(AST) ->
   erlang:list_to_binary(to_string(AST)).

compare({ident, Ident, _}, {ident, Ident, _}) ->
    true;
compare({char, Char, _}, {char, Char, _}) ->
    true;
compare({fun_ref, Spec, _}, {fun_ref, Spec, _}) ->
    true;
compare({integer, I, _}, {integer, I, _}) ->
    true;
compare({float, F, _}, {float, F, _}) ->
    true;
compare({string, S, _}, {string, S, _}) ->
    true;
compare({quote, Value1, _}, {quote, Value2, _}) ->
    compare(Value1, Value2);
compare({vector, V1, _}, {vector, V2, _}) ->
    lists:all(fun({I1, I2}) ->
                      compare(I1, I2)
              end, lists:zip(V1, V2));
compare({literal_list, V1, _}, {literal_list, V2, _}) ->
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
                {ParsedExpr, _, _} = jxa_parser:intermediate_parse(BinExpr),
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
           integer(42, 46),
           integer(60, 90),
           integer(94, 95),
           integer(97, 122),
           integer(126, 127)]).

ident_character() ->
    union([33,
           integer(35, 38),
           integer(42, 46),
           integer(60, 90),
           integer(94, 122)]).

ident_string() ->
    ?LET({S1, S2},
         {ident_initial(),
          list([ident_character()])},
         [S1 | erlang:binary_to_list(unicode:characters_to_binary(S2))]).

ident_atom() ->
    ?LET(S, ident_string(),
         list_to_atom(S)).

defvar_style_ident() ->
    ?LET(S, ident_string(),
         list_to_atom("*" ++ S ++ "*")).

split_ident() ->
    ?LET({S1, S2}, {ident_string(), ident_string()},
         list_to_atom(S1 ++ "-" ++ S2)).

normal_ident() ->
    ident_atom().

symbol() ->
   ?LET(I, normal_ident(),
        {quote, {ident, I, 0}, 0}).

ident() ->
    {ident, union([normal_ident(),
                   split_ident(),
                   defvar_style_ident()]), 0}.

character() ->
    ?LET(Char, string_character(),
         {char, list_to_binary([Char]), 0}).

jxa_int() ->
    {integer, integer(), 0}.

jxa_float() ->
    {float, float(), 0}.

jxa_string() ->
    {string, internal_string(), 0}.

jxa_vector(0) ->
    {vector, [], 0};
jxa_vector(Size) ->
    {vector, resize(Size, list(value(Size div 4))), 0}.

jxa_list(0) ->
    {list, [], 0};
jxa_list(Size) ->
    {list, resize(Size, list(value(Size div 4))), 0}.

jxa_literal_list(0) ->
    {list, [], 0};
jxa_literal_list(Size) ->
    {literal_list, resize(Size, list(value(Size div 4))), 0}.

jxa_quote(0) ->
    {list, [], 0};
jxa_quote(Size) ->
    {quote, value(Size div 4), 0}.

value(Size) ->
    union([jxa_quote(Size),
           jxa_vector(Size),
           jxa_list(Size),
           jxa_literal_list(Size),
           jxa_float(),
           jxa_int(),
           jxa_string(),
           character(),
           symbol(),
           ident()]).

expression() ->
    ?SIZED(N,
           value(N)).

