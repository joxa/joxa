%% -*- mode: Erlang; fill-column: 80; comment-column: 76; -*-
%%%-------------------------------------------------------------------
%%% Permission is hereby granted, free of charge, to any
%%% person obtaining a copy of this software and associated
%%% documentation files (the "Software"), to deal in the
%%% Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute,
%%% sublicense, and/or sell copies of the Software, and to permit
%%% persons to whom the Software is furnished to do so, subject to
%%% the following conditions:
%%%
%%% The above copyright notice and this permission notice shall
%%% be included in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%%% OTHER DEALINGS IN THE SOFTWARE.
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt
%%% @copyright (C) 2011, Erlware, LLC.
%%% @doc
%%%  Parser for the joxa language
%%% @end
%%%-------------------------------------------------------------------
-module(jxa_parser).


-export([file/1, parse/2]).

%% for testing purposes
-export([intermediate_parse/1, p_charclass/1]).

-include_lib("joxa/include/joxa.hrl").

-export_type([ast/0, type_desc/0, annotation/0]).

%%=============================================================================
%% Types
%%=============================================================================
-type type_desc() :: char |
                     string |
                     list |
                     literal_list |
                     tuple |
                     call |
                     quote |
                     float |
                     integer.

-type raw_type() :: char() | string() | list() |
                    float() | integer().

-type intermediate_ast() :: {char, char(), index()} |
                            {string, string(), index()} |
                            {quote, intermediate_ast(), index()} |
                            {list, [intermediate_ast()], index()} |
                            {literal_list, [intermediate_ast()], index()} |
                            {tuple, [intermediate_ast()], index()} |
                            {call,
                             {atom(), atom(), non_neg_integer()} |
                             {atom(), non_neg_integer()} |
                             {atom(), atom()}, index()} |

                            {float, float(), index()} |
                            {integer, integer(), index()}.

-type ast() :: [ast()] |
               tuple(ast()) |
               integer() |
               float() |
               atom().

-type annotation() :: {type_desc(), index()}.

-type index() :: {non_neg_integer(), non_neg_integer()}.

%%=============================================================================
%% API
%%=============================================================================
-spec file(string()) -> intermediate_ast().
file(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    parse(Filename, Bin).

-spec intermediate_parse(binary()) -> intermediate_ast() | fail.
intermediate_parse(Input) when is_binary(Input) ->
    intermediate_parse(Input, new_index()).

-spec parse(Filename::string(), binary()) ->
                   {[{Type::atom(), Value::term(), index()}],
                    jxa_annots:annotations()}.
parse(Filename, Input) ->
    do_parse(jxa_path:new(),
             jxa_annot:new(Filename), [], Input, new_index()).

%%=============================================================================
%% Internal Functions
%%=============================================================================
-spec do_parse(jxa_path:path(),
               jxa_annots:annotations(), [term()], binary(),
               index()) ->
                      {[{Type::atom(), Value::term(), index()}],
                       jxa_annots:annotations()}.
do_parse(_Path0, Annots, Ast, <<>>, _Idx) ->
    {Annots, lists:reverse(Ast)};
do_parse(Path0, Annots0, Ast, Input, Idx0) ->
    case intermediate_parse(Input, Idx0) of
        {fail,{expected, Expected, Idx}} ->
            ?JXA_THROW({invalid_form, Expected, Idx});
        {IntermediateAst, Rest, Idx1} ->
            {Annots1, FinalAST} =
                transform_ast(jxa_path:add(Path0),
                              Annots0, IntermediateAst),
            do_parse(jxa_path:incr(Path0), Annots1,
                     [FinalAST | Ast],
                     Rest, Idx1)

    end.

-spec transform_ast(jxa_path:state(), jxa_annot:annotations(),
                    fail | intermediate_ast()) ->
                           {jxa_annot:annotations(), raw_type()}.
transform_ast(_, _, fail) ->
    erlang:throw(fail);
transform_ast(Path0, Annotations, {call, MFA, Idx}) ->
    {jxa_annot:add(jxa_path:path(Path0),
                   call, Idx, Annotations),
     MFA};
transform_ast(Path0, Annotations0, {literal_list, List, Idx}) ->
    {_, Annotations3, TransformList} =
        lists:foldl(fun(El, {Path1, Annotations1, Elements}) ->
                            {Annotations2, Transformed} =
                                transform_ast(jxa_path:add(Path1),
                                              Annotations1, El),
                            Path2 = jxa_path:incr(Path1),
                            {Path2, Annotations2, [Transformed | Elements]}
                    end, {jxa_path:incr(Path0), Annotations0, []}, List),
    %% We need to put in the annotation for the programatically inserted
    %% 'list' argument
    Annotations4 =
        jxa_annot:add(jxa_path:add_path(Path0),
                      ident, Idx,
                      Annotations3),
    {jxa_annot:add(jxa_path:path(Path0),
                   literal_list, Idx,
                   Annotations4),
     [list | lists:reverse(TransformList)]};
transform_ast(Path0, Annotations0,
              {binary, {string, String,_}, Idx})->
    {jxa_annot:add(jxa_path:path(Path0),
                   binary, Idx, Annotations0),
     erlang:list_to_binary(String)};
transform_ast(Path0, Annotations0, {binary, List, Idx}) ->
    {_, Annotations3, TransformList} =
        lists:foldl(fun(El, {Path1, Annotations1, Elements}) ->
                            {Annotations2, Transformed} =
                                transform_ast(jxa_path:add(Path1),
                                              Annotations1, El),
                            Path2 = jxa_path:incr(Path1),
                            {Path2, Annotations2, [Transformed | Elements]}
                    end, {jxa_path:incr(Path0), Annotations0, []}, List),
    %% We need to put in the annotation for the programatically inserted
    %% 'list' argument
    Annotations4 = jxa_annot:add(jxa_path:add_path(Path0), ident, Idx,
                                 Annotations3),
    {jxa_annot:add(jxa_path:path(Path0), literal_list, Idx, Annotations4),
     [binary | lists:reverse(TransformList)]};
transform_ast(Path0, Annotations0, {tuple, List, Idx}) ->
    {_, Annotations3, TransformList} =
        lists:foldl(fun(El, {Path1, Annotations1, Elements}) ->
                            {Annotations2, Transformed} =
                                transform_ast(jxa_path:add(Path1),
                                              Annotations1, El),
                            Path2 = jxa_path:incr(Path1),
                            {Path2, Annotations2, [Transformed | Elements]}
                    end, {Path0, Annotations0, []}, List),
    {jxa_annot:add(jxa_path:path(Path0), tuple, Idx, Annotations3),
     list_to_tuple(lists:reverse(TransformList))};
transform_ast(Path0, Annotations0, {list, List, Idx}) ->
    {_, Annotations3, TransformList} =
        lists:foldl(fun(El, {Path1, Annotations1, Elements}) ->
                            {Annotations2, Transformed} =
                                transform_ast(jxa_path:add(Path1),
                                              Annotations1, El),
                            Path2 = jxa_path:incr(Path1),
                            {Path2, Annotations2, [Transformed | Elements]}
                    end, {Path0, Annotations0, []}, List),
    {jxa_annot:add(jxa_path:path(Path0), list, Idx, Annotations3),
     lists:reverse(TransformList)};
transform_ast(Path0, Annotations0, {Type, Val={_, _, _}, Idx}) ->
    {Annotations1, PVal} =
        transform_ast(jxa_path:add(jxa_path:incr(Path0)),
                      Annotations0, Val),
    Annotations2 = jxa_annot:add(jxa_path:add(Path0), ident, Idx,
                                 Annotations1),
    {jxa_annot:add(jxa_path:path(Path0),
                   Type, Idx, Annotations2), [Type, PVal]};

transform_ast(Path0, Annotations, {Type, Val, Idx}) ->
    {jxa_annot:add(jxa_path:path(Path0), Type, Idx, Annotations),
     Val}.

-spec intermediate_parse(string(), index()) -> intermediate_ast() | fail.
intermediate_parse(Input, Index) when is_binary(Input) ->
    setup_memo(),
    Result = value(Input, Index),
    release_memo(),
    Result.

-spec index() -> index().
new_index() ->
    {1, 1}.

%% done

-spec integer(binary(), index()) -> intermediate_ast().
integer(Input, Index) ->
    p(Input, Index, integer,
      fun int_part/2,
      fun(Node, Idx) ->
              Result =
                  list_to_integer(binary_to_list(iolist_to_binary(Node))),
              {integer, Result, Idx}
      end).

%% done
-spec float(binary(), index()) -> intermediate_ast().
float(Input, Index) ->
    p(Input, Index, float,
      p_seq([fun int_part/2,
             fun frac_part/2,
             p_optional(fun exp_part/2)]),
      fun(Node, Idx) ->
              Result =
                  list_to_float(binary_to_list(iolist_to_binary(Node))),
              {float, Result, Idx}
      end).

%% done
-spec char(binary(), index()) -> intermediate_ast().
char(Input, Index) ->
    p(Input, Index, char,
      p_seq([p_string(<<"\\">>),
             p_anything()]),
      fun([_, Char], Idx) ->
              {char, hd(binary_to_list(Char)), Idx}
      end).

%% done
-spec int_part(binary(), index()) -> intermediate_ast().
int_part(Input, Index) ->
    p(Input, Index, int_part,
      p_seq([p_optional(p_string(<<"-">>)),
             p_one_or_more(fun digit/2)])).

%% Done
-spec frac_part(binary(), index()) -> intermediate_ast().
frac_part(Input, Index) ->
    p(Input, Index, frac_part,
      fun(I,D) ->
              (p_seq([p_string(<<".">>), p_one_or_more(fun digit/2)]))(I,D)
      end).

%% done
-spec exp_part(binary(), index()) -> intermediate_ast().
exp_part(Input, Index) ->
    p(Input, Index, exp_part,
      fun(I,D) ->
              (p_seq([fun 'e'/2, p_one_or_more(fun 'digit'/2)]))(I,D)
      end).

%% done
-spec e(binary(), index()) -> intermediate_ast().
e(Input, Index) ->
    p(Input, Index, e,
      fun(I,D) ->
              (p_seq([p_charclass(<<"[eE]">>),
                      p_optional(p_choose([p_string(<<"+">>),
                                           p_string(<<"-">>)]))]))(I,D)
      end).
%% done
digit(Input, Index) ->
    p(Input, Index, digit,
      fun(I,D) ->
              (p_charclass(<<"[0-9]">>))(I,D)
      end).

-spec binary(binary(), index()) -> intermediate_ast().
binary(Input, Index) ->
    p(Input, Index, binary,
      p_choose([p_seq([p_string(<<"<<">>),
                       fun ignorable/2,
                       p_choose([fun integer/2,
                                 fun char/2,
                                 fun ident/2,
                                 fun list/2]),
                       p_zero_or_more(p_seq([fun ignorable/2,
                                             p_choose([fun integer/2,
                                                       fun char/2,
                                                       fun ident/2,
                                                       fun list/2])])),
                       fun ignorable/2,
                       p_string(<<">>">>)]),
                p_seq([p_string(<<"<<">>),
                       fun ignorable/2,
                       fun string/2,
                       fun ignorable/2,
                       p_string(<<">>">>)]),
                p_seq([p_string(<<"<<">>),
                       fun ignorable/2,
                       p_string(<<">>">>)])]),
      fun([_, _, H, T, _, _], Idx) ->
              {binary, lists:flatten([H, T]), Idx};
         ([_, _, String, _, _], Idx) ->
              {binary, String, Idx};
         ([_, _, _], Idx) ->
              {binary, [], Idx}
      end).

-spec tuple(binary(), index()) -> intermediate_ast().
tuple(Input, Index) ->
    p(Input, Index, tuple,
      p_choose([p_seq([p_string(<<"{">>),
                       fun ignorable/2,
                       fun value/2,
                       p_zero_or_more(p_seq([fun ignorable/2,
                                             fun value/2])),
                       fun ignorable/2,
                       p_string(<<"}">>)]),
                p_seq([p_string(<<"{">>),
                       fun ignorable/2,
                       p_string(<<"}">>)])]),
      fun([_, _, H, T, _, _], Idx) ->
              {tuple, lists:flatten([H, T]), Idx};
         ([_, _, _], Idx) ->
              {tuple, [], Idx}
      end).

-spec list(binary(), index()) -> intermediate_ast().
list(Input, Index) ->
    p(Input, Index, list,
      p_choose([p_seq([p_string(<<"(">>),
                       fun ignorable/2,
                       fun value/2,
                       p_zero_or_more(p_seq([fun ignorable/2,
                                             fun value/2])),
                       fun ignorable/2,
                       p_string(<<")">>)]),
                p_seq([p_string(<<"(">>),
                       fun ignorable/2,
                       p_string(<<")">>)]),
                p_seq([p_string(<<"[">>),
                       fun ignorable/2,
                       fun value/2,
                       p_zero_or_more(p_seq([fun ignorable/2,
                                             fun value/2])),
                       fun ignorable/2,
                       p_string(<<"]">>)]),
                p_seq([p_string(<<"[">>),
                       fun ignorable/2,
                       p_string(<<"]">>)])]),
      fun([<<"(">>, _, H, T, _, _], Idx) ->
              {list, lists:flatten([H, T]), Idx};
         ([<<"[">>, _, H, T, _, _], Idx) ->
              {literal_list, lists:flatten([H, T]), Idx};
         ([<<"[">>, _, _], Idx) ->
              {literal_list, [], Idx};
         ([_, _, _], Idx) ->
              {list, [], Idx}
      end).

-spec string(binary(), index()) -> intermediate_ast().
string(Input, Index) ->
    p(Input, Index, string,
      p_seq([p_string(<<"\"">>),
             p_zero_or_more(p_seq([p_not(p_string(<<"\"">>)),
                                   p_choose([p_string(<<"\\\"">>),
                                             p_string(<<"\\\\">>),
                                             p_string(<<"\\b">>),
                                             p_string(<<"\\f">>),
                                             p_string(<<"\\n">>),
                                             p_string(<<"\\r">>),
                                             p_string(<<"\\t">>),
                                             p_anything()])])),
             p_string(<<"\"">>)]),
      fun([_, Node, _], Idx) ->
              Result =
                  binary_to_list(iolist_to_binary(lists:map(fun([_, <<"\\\"">>]) ->
                                                                    <<"\"">>;
                                                               ([_, <<"\\\\">>]) ->
                                                                    <<"\\">>;
                                                               ([_, <<"\\b">>]) ->
                                                                    <<"\b">>;
                                                               ([_, <<"\\f">>]) ->
                                                                    <<"\f">>;
                                                               ([_, <<"\\n">>]) ->
                                                                    <<"\n">>;
                                                               ([_, <<"\\r">>]) ->
                                                                    <<"\r">>;
                                                               ([_, <<"\\t">>]) ->
                                                                    <<"\t">>;
                                                               (El)  ->
                                                                    El
                                                            end,
                                                            Node))),
              {string, Result, Idx}
      end).

-spec space(binary(), index()) -> intermediate_ast().
space(Input, Index) ->
    p(Input, Index, space,
      p_charclass(<<"[ \t\n\s\r]">>)).

-spec eol(binary(), index()) -> intermediate_ast().
eol(Input, Index) ->
    p(Input, Index, eol,
      p_charclass(<<"[\n\r]">>)).

-spec comment(binary(), index()) -> intermediate_ast().
comment(Input, Index) ->
    p(Input, Index, comment,
      p_seq([p_string(";"),
             p_zero_or_more(p_charclass(<<"[^\n\r]">>)),
             p_choose([fun eol/2,
                       p_eof()])])).

-spec ignorable(binary(), index()) -> intermediate_ast().
ignorable(Input, Index) ->
    p(Input, Index, ignorable,
      p_optional(p_zero_or_more(p_choose([fun space/2,
                                          p_string(","),
                                          fun comment/2]))),
      fun(_, _Idx) ->
              []
      end).

fun_reference(Input, Index) ->
    p(Input, Index, fun_reference,
      p_choose([p_seq([fun ident/2,
                       p_string("/"),
                       fun ident/2,
                       p_string("/"),
                       fun integer/2]),
                p_seq([fun ident/2,
                       p_string("/"),
                       fun integer/2]),
                p_seq([fun ident/2,
                       p_string("/"),
                       fun ident/2])]),
      fun([{ident, Module, _}, _,
           {ident, Function, _}, _,
           {integer, Arity, _}], Idx) ->
              {call, {'__fun__', Module, Function, Arity}, Idx};
         ([{ident, Function, _}, _,
           {integer, Arity, _}], Idx) ->
              {call, {'__fun__', Function, Arity}, Idx};
         ([{ident, Module, _}, _,
           {ident, Function, _}], Idx) ->
              {call, {'__fun__', Module, Function}, Idx}
      end).

-spec symbol(binary(), index()) -> intermediate_ast().
symbol(Input, Index) ->
    p(Input, Index, symbol,
      p_seq([p_string(":"),
             fun ident/2]),
      fun([_, Result = {ident, _, _}], Idx) ->
              {quote, Result, Idx}
      end).

-spec ident(binary(), index()) -> intermediate_ast().
ident(Input, Index) ->
    p(Input, Index, ident,
      p_choose([p_string("/"),
                p_one_or_more(
                  p_and([p_not(
                           p_charclass(<<"[ :;~`'\\\\,><{}/\t\n\s\r\\(\\)\\[\\]\"]">>)),
                         p_anything()]))]),

      fun(Node, Idx) ->
              Result =
                  list_to_atom(binary_to_list(iolist_to_binary(Node))),
              {ident, Result, Idx}
      end).

-spec quote(binary(), index()) -> intermediate_ast().
quote(Input, Index) ->
    p(Input, Index, quote,
      p_seq([p_string("'"),
             fun value/2]),

      fun([_, Item], Idx) ->
              {quote, Item, Idx}
      end).

-spec value(binary(), index()) -> intermediate_ast().
value(Input, Index) ->
    p(Input, Index, value,
      fun(I,D) ->
              (p_seq([fun ignorable/2,
                      p_choose([fun float/2,
                                fun integer/2,
                                fun fun_reference/2,
                                fun ident/2,
                                fun symbol/2,
                                fun list/2,
                                fun tuple/2,
                                fun string/2,
                                fun quote/2,
                                fun char/2,
                                fun binary/2]),
                      fun ignorable/2]))(I,D) end,
      fun(Node, _Idx) ->
              lists:nth(2, Node)
      end).

p(Inp, Index, Name, ParseFun) ->
    p(Inp, Index, Name, ParseFun, fun(N, _Idx) -> N end).

p(Inp, StartIndex, Name, ParseFun, TransformFun) ->
    %% See if the current reduction is memoized
    case get_memo(StartIndex, Name) of
        %% If it is, return the stored result
        {ok, Memo} ->
            Memo;
        _ ->
            %% If not, attempt to parse
            Result =
                case ParseFun(Inp, StartIndex) of
                    %% If it fails, memoize the failure
                    {fail,_} = Failure ->
                        Failure;
                    %% If it passes, transform and memoize the result.
                    {Match, InpRem, NewIndex} ->
                        Transformed = TransformFun(Match, StartIndex),
                        {Transformed, InpRem, NewIndex}
                end,
            memoize(StartIndex, Name, Result),
            Result
    end.

setup_memo() ->
    put(parse_memo_table, ets:new(?MODULE, [set])).

release_memo() ->
    ets:delete(memo_table()).

memoize(Index, Name, Result) ->
    Memo = case ets:lookup(memo_table(), Index) of
               [] -> [];
               [{Index, Plist}] -> Plist
           end,
    ets:insert(memo_table(), {Index, [{Name, Result}|Memo]}).

get_memo(Index, Name) ->
    case ets:lookup(memo_table(), Index) of
        [] -> {error, not_found};
        [{Index, Plist}] ->
            case proplists:lookup(Name, Plist) of
                {Name, Result}  -> {ok, Result};
                _  -> {error, not_found}
            end
    end.

memo_table() ->
    get(parse_memo_table).

p_optional(P) ->
    fun(Input, Index) ->
            case P(Input, Index) of
                {fail,_} -> {[], Input, Index};
                {_, _, _} = Success -> Success
            end
    end.

p_not(P) ->
    fun(Input, Index)->
            case P(Input,Index) of
                {fail,_} ->
                    {[], Input, Index};
                {Result, _, _} -> {fail, {expected, {no_match, Result},Index}}
            end
    end.

p_and(P) ->
    p_seq(P).

p_seq(P) ->
    fun(Input, Index) ->
            p_all(P, Input, Index, [])
    end.

p_all([], Inp, Index, Accum ) -> {lists:reverse( Accum ), Inp, Index};
p_all([P|Parsers], Inp, Index, Accum) ->
    case P(Inp, Index) of
        {fail, _} = Failure -> Failure;
        {Result, InpRem, NewIndex} ->
            p_all(Parsers, InpRem, NewIndex, [Result|Accum])
    end.

p_choose(Parsers) ->
    fun(Input, Index) ->
            p_attempt(Parsers, Input, Index, none)
    end.

p_attempt([], _Input, _Index, Failure) -> Failure;
p_attempt([P|Parsers], Input, Index, FirstFailure)->
    case P(Input, Index) of
        {fail, _} = Failure ->
            case FirstFailure of
                none -> p_attempt(Parsers, Input, Index, Failure);
                _ -> p_attempt(Parsers, Input, Index, FirstFailure)
            end;
        Result -> Result
    end.

p_zero_or_more(P) ->
    fun(Input, Index) ->
            p_scan(P, Input, Index, [])
    end.

p_one_or_more(P) ->
    fun(Input, Index)->
            Result = p_scan(P, Input, Index, []),
            case Result of
                {[_|_], _, _} ->
                    Result;
                _ ->
                    {fail, {expected, Failure, _}} = P(Input,Index),
                    {fail, {expected, {at_least_one, Failure}, Index}}
            end
    end.

p_scan(_, [], Index, Accum) -> {lists:reverse( Accum ), [], Index};
p_scan(P, Inp, Index, Accum) ->
    case P(Inp, Index) of
        {fail,_} -> {lists:reverse(Accum), Inp, Index};
        {Result, InpRem, NewIndex} ->
            p_scan(P, InpRem, NewIndex, [Result | Accum])
    end.

p_string(S) when is_list(S) -> p_string(list_to_binary(S));
p_string(S) ->
    Length = erlang:byte_size(S),
    fun(Input, Index) ->
            try
                <<S:Length/binary, Rest/binary>> = Input,
                {S, Rest, p_advance_index(S, Index)}
            catch
                error:{badmatch,_} -> {fail, {expected, {string, S}, Index}}
            end
    end.

p_anything() ->
    fun(<<>>, Index) -> {fail, {expected, any_character, Index}};
       (Input, Index) when is_binary(Input) ->
            <<C/utf8, Rest/binary>> = Input,
            {<<C/utf8>>, Rest, p_advance_index(<<C/utf8>>, Index)}
    end.

p_charclass(Class) ->
    {ok, RE} = re:compile(Class, [unicode, dotall]),
    fun(Inp, Index) ->
            case re:run(Inp, RE, [anchored]) of
                {match, [{0, Length}|_]} ->
                    {Head, Tail} = erlang:split_binary(Inp, Length),
                    {Head, Tail, p_advance_index(Head, Index)};
                _ ->
                    {fail, {expected, {character_class,
                                       binary_to_list(Class)}, Index}}
            end
    end.

p_eof() ->
    fun(<<>>, Index) -> {eof, <<>>, Index};
       (_, Index) -> {fail, {expected, eof, Index}} end.

p_advance_index(MatchedInput, Index)
  when is_list(MatchedInput) orelse is_binary(MatchedInput)->
    lists:foldl(fun p_advance_index/2,
                Index,
                unicode:characters_to_list(MatchedInput));
p_advance_index(MatchedInput, Index)
  when is_integer(MatchedInput) ->
    {Line, Col} = Index,
    case MatchedInput of
        $\n -> {Line+1, 1};
        _ -> {Line, Col+1}
    end.

%%=============================================================================
%% Unit tests
%%=============================================================================
-ifndef(NOTEST).
-include_lib("eunit/include/eunit.hrl").

index() ->
    {1, 1}.

-define(memo(X), setup_memo(), X, release_memo()).

number_test() ->
    ?memo(?assertMatch({{integer, 44, {1, _}}, <<>>, _}, value(<<"44">>, index()))),
    ?memo(?assertMatch({{integer, -44, {1, _}}, <<>>, _}, value(<<"-44">>, index()))),
    ?memo(?assertMatch({{float, 44.00, {1, _}}, <<>>, _}, value(<<"44.00">>, index()))),
    ?memo(?assertMatch({{float, -44.01, {1, _}}, <<>>, _}, value(<<"-44.01">>, index()))),
    ?memo(?assertMatch({{float, 44.00e+33, {1, _}}, <<>>, _},
                       value(<<"44.00e+33">>, index()))),
    ?memo(?assertMatch({{float, 44.00e33, {1, _}}, <<>>, _}, value(<<"44.00e33">>, index()))),
    ?memo(?assertMatch({{float, 44.00e-10, {1, _}}, <<>>, _},
                       value(<<"44.00e-10">>,  index()))),
    ?memo(?assertMatch({{float, 42.44, {1, _}}, <<>>, _}, value(<<"42.44">>, index()))),
    ?memo(?assertMatch({{float, 41.33, {1, _}}, <<>>, _}, value(<<"41.33">>, index()))),
    ?memo(?assertMatch({{integer, 0, {1, _}}, <<>>, _}, value(<<"0">>, index()))),
    ?memo(?assertMatch({{float, -0.1, {1, _}}, <<>>, _}, value(<<"-0.1">>, index()))).

string_test() ->
    ?memo(?assertMatch({{string, "Hello World", {1, _}}, <<>>, _},
                       value(<<"\"Hello World\"">>, index()))),
    ?memo(?assertMatch({{string, "Hello\n World", {1, _}}, <<>>, _},
                       value(<<"\"Hello\n World\"">>, index()))),
    ?memo(?assertMatch({{string,"Hello \\\" World", {1, _}}, <<>>, _},
                       value(<<"\"Hello \\\\\\\" World\"">>, index()))),
    ?memo(?assertMatch({{string, "Hello\\ World", {1, _}}, <<>>, _},
                       value(<<"\"Hello\\ World\"">>, index()))),
    ?memo(?assertMatch({{string, "Hello\/ World", {1, _}}, <<>>, _},
                       value(<<"\"Hello\/ World\"">>, index()))),
    ?memo(?assertMatch({{string, "Hello\b World", {1, _}}, <<>>, _},
                       value(<<"\"Hello\b World\"">>, index()))),
    ?memo(?assertMatch({{string, "Hello\f World", {1, _}}, <<>>, _},
                       value(<<"\"Hello\f World\"">>, index()))),
    ?memo(?assertMatch({{string, "Hello\n World", {1, _}}, <<>>, _},
                       value(<<"\"Hello\n World\"">>, index()))),
    ?memo(?assertMatch({{string, "Hello\r World", {1, _}}, <<>>, _},
                       value(<<"\"Hello\r World\"">>, index()))),
    ?memo(?assertMatch({{string, "Hello\t World", {1, _}}, <<>>, _},
                       value(<<"\"Hello\t World\"">>, index()))).

ident_test() ->
    ?memo(?assertMatch({{ident, '/', {1, _}}, <<>>, _},
                       value(<<"/">>, index()))),
    ?memo(?assertMatch({{ident, 'true', {1, _}}, <<>>, _}, value(<<"true">>, index()))),
    ?memo(?assertMatch({{ident, 'false', {1, _}},  <<>>, _},value(<<"false">>, index()))),
    ?memo(?assertMatch({{quote, {ident, 'keyword', _}, {1, _}}, <<>>, _},
                       value(<<":keyword">>, index()))),
    ?memo(?assertMatch({{ident, '*foo*', {1, _}}, <<>>, _}, value(<<"*foo*">>, index()))),
    ?memo(?assertMatch({{ident, 'foo-bar', {1, _}}, <<>>, _}, value(<<"foo-bar">>, index()))),
    ?memo(?assertMatch({{ident, 'null', {1, _}}, <<>>, _}, value(<<"null">>, index()))),
    ?memo(?assertMatch({{ident, 'Hello?', {1, _}}, <<>>, _}, value(<<"Hello?">>, index()))),
    ?memo(?assertMatch({{ident, 'boo88', {1, _}}, <<>>, _}, value(<<"boo88">>, index()))),
    ?memo(?assertMatch({{ident, 'bock', {1, _}}, <<":">>, _}, value(<<"bock:">>, index()))),
    ?memo(?assertMatch({{ident, 'bock', {1, _}}, <<"{">>, _}, value(<<"bock{">>, index()))),
    ?memo(?assertMatch({{ident, 'bock', {1, _}}, <<"[">>, _}, value(<<"bock[">>, index()))),
    ?memo(?assertMatch({{ident, 'bock', {1, _}}, <<"(ee">>, _}, value(<<"bock(ee">>, index()))).

parse_test() ->
    Value = list_to_binary("(io/format \n \"~p\" \n '(\n(foo \n bar \n baz 33)))"),
    {Annots, Result} =  parse("", Value),
    ?assertMatch([[{'__fun__', 'io', 'format'}, "~p",
                   [quote, [[foo, bar, baz, 33]]]]], Result),

    ?assertMatch({call, {1, 2}, _},
                 jxa_annot:get([1,1], Annots)),
    ?assertMatch({string, {2, 2}, _},
                 jxa_annot:get([2,1], Annots)),
    ?assertMatch({quote, {3, 2}, _},
                 jxa_annot:get([3, 1], Annots)),
    ?assertMatch({ident, {4, 2}, _},
                 jxa_annot:get([1, 1, 2, 3, 1], Annots)).

-endif.
