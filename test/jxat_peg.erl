%%%-------------------------------------------------------------------
%%% @author Eric B Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2011, Eric B Merritt
%%% @doc
%%%
%%% @end
%%% Created : 29 Dec 2011 by Eric B Merritt <ericbmerritt@gmail.com>
%%%-------------------------------------------------------------------
-module(jxat_peg).

%% API
-include_lib("eunit/include/eunit.hrl").

-define(memo(X),
        joxa.compiler:'setup-memo'(),
        X,
        joxa.compiler:'release-memo'()).

%%%===================================================================
%%% Tests
%%%===================================================================
line_test() ->
    ?assertMatch(100, joxa.compiler:line({100, 0})),
    ?assertError({case_clause,foo}, joxa.compiler:line(foo)).

column_test() ->

    ?assertMatch(56, joxa.compiler:column({100, 56})),
    ?assertError({case_clause,foo}, joxa.compiler:column(foo)).

p_charclass_test() ->
    Fun = joxa.compiler:'p-charclass'("abc"),
    ?assertMatch(true, erlang:is_function(Fun)),
    ?assertMatch({<<"abc">>,<<"123">>,{1, 4}},
                 Fun(<<"abc123">>, index())),
    ?assertMatch({fail,{expected,{'character-class',"abc"},{1,1}}},
                 Fun(<<"123">>, index())),
    ?assertMatch({<<"abc">>,<<"\n123">>,{1,4}},
                 Fun(<<"abc\n123">>, index())).

p_anything_test() ->
    Fun = joxa.compiler:'p-anything'(),
    ?assertMatch(true, erlang:is_function(Fun)),
    ?assertMatch({97,<<"bc123">>,{1, 2}},
                 Fun(<<"abc123">>, index())),
    ?assertMatch({fail,{expected, 'any-character', {1,1}}},
                 Fun(<<>>, index())),
    ?assertMatch({97,<<"bc\n123">>,{1,2}},
                 Fun(<<"abc\n123">>, index())).
p_string_test() ->
    FunS = joxa.compiler:'p-string'("foo"),
    FunB = joxa.compiler:'p-string'(<<"foo">>),

    ?assertMatch(true, erlang:is_function(FunS)),
    ?assertMatch(true, erlang:is_function(FunB)),
    ?assertMatch({<<"foo">>,<<" bar">>,{1, 4}},
                 FunS(<<"foo bar">>, index())),
    ?assertMatch({<<"foo">>,<<" bar">>,{1, 4}},
                 FunB(<<"foo bar">>, index())),
    ?assertMatch({fail,{expected, {string, <<"foo">>}, {1,1}}},
                 FunS("bar", index())),
    ?assertMatch({fail,{expected, {string, <<"foo">>}, {1,1}}},
                 FunB("bar", index())).

p_scan_test() ->
    FunS = joxa.compiler:'p-string'("foo"),
    ?assertMatch(true, erlang:is_function(FunS)),
    ?assertMatch({[<<"foo">>],<<" 123">>,{1,4}},
                 joxa.compiler:'p-scan'(FunS,
                                        <<"foo 123">>,
                                        index(),
                                        [])),
    ?assertMatch({[],<<"abc 123">>,{1,1}},
                 joxa.compiler:'p-scan'(FunS,
                                        <<"abc 123">>,
                                        index(),
                                        [])),
    ?assertMatch({[],<<"12c 123">>,{1,1}},
                 joxa.compiler:'p-scan'(FunS,
                                        <<"12c 123">>,
                                        index(),
                                        [])).
p_one_or_more_test() ->
    FunS = joxa.compiler:'p-string'("("),
    FunT = joxa.compiler:'p-one-or-more'(FunS),
    ?assertMatch(true, erlang:is_function(FunS)),
    ?assertMatch(true, erlang:is_function(FunT)),
    ?assertMatch({[<<"(">>,<<"(">>,<<"(">>,<<"(">>,<<"(">>,<<"(">>],
                  <<>>,
                  {1,7}},
                 FunT(<<"((((((">>, index())),
    ?assertMatch({fail, {expected,{'at-least-one',{string,<<"(">>}},{1,1}}},
                 FunT(<<"--((((">>, index())),
    ?assertMatch({fail,{expected,{'at-least-one',{string,<<"(">>}},{1,1}}},
                 FunT(<<"ok">>, index())).

p_zero_or_more_test() ->
    FunS = joxa.compiler:'p-string'("foo"),
    FunT = joxa.compiler:'p-zero-or-more'(FunS),
    ?assertMatch(true, erlang:is_function(FunS)),
    ?assertMatch(true, erlang:is_function(FunT)),
    ?assertMatch({[<<"foo">>],<<" 123">>,{1,4}},
                 FunT(<<"foo 123">>, index())),
    ?assertMatch({[],<<"abc 123">>,{1,1}},
                 FunT(<<"abc 123">>, index())),

    ?assertMatch({[],<<"12c 123">>,{1,1}},
                 FunT(<<"12c 123">>, index())).


p_attempt_test() ->
    FunS = joxa.compiler:'p-string'(<<"(">>),
    FunC = joxa.compiler:'p-charclass'(<<"abc">>),
    ?assertMatch(true, erlang:is_function(FunS)),
    ?assertMatch(true, erlang:is_function(FunC)),
    ?assertMatch({<<"abc">>,<<"(((">>,{1,4}},
                 joxa.compiler:'p-attempt'([FunS, FunC],
                                           <<"abc(((">>, index(),
                                           none)),
    ?assertMatch({<<"(">>,<<"((abc">>,{1,2}},
                 joxa.compiler:'p-attempt'([FunS, FunC],
                                           <<"(((abc">>, index(),
                                           none)),
    ?assertMatch({fail,{expected,{string,<<"(">>},{1,1}}},
                 joxa.compiler:'p-attempt'([FunS, FunC],
                                           <<"xxxx">>, index(),
                                           none)).

p_choose_test() ->
    FunS = joxa.compiler:'p-string'(<<"(">>),
    FunC = joxa.compiler:'p-charclass'(<<"abc">>),
    FunT = joxa.compiler:'p-choose'([FunS, FunC]),
    ?assertMatch(true, erlang:is_function(FunS)),
    ?assertMatch(true, erlang:is_function(FunC)),
    ?assertMatch({<<"abc">>,<<"(((">>,{1,4}},
                 FunT(<<"abc(((">>, index())),
    ?assertMatch({<<"(">>,<<"((abc">>,{1,2}},
                 FunT(<<"(((abc">>, index())),
    ?assertMatch({fail,{expected,{string,<<"(">>},{1,1}}},
                 FunT(<<"xxxx">>, index())).


p_all_test() ->
    FunS = joxa.compiler:'p-string'(<<"((">>),
    FunC = joxa.compiler:'p-charclass'(<<"abc">>),
    ?assertMatch(true, erlang:is_function(FunS)),
    ?assertMatch(true, erlang:is_function(FunC)),
    ?assertMatch({[<<"abc">>,<<"((">>],<<>>,{1,6}},
                 joxa.compiler:'p-all'([FunC, FunS],
                                       <<"abc((">>, index(),
                                       [])),
    ?assertMatch({[<<"abc">>,<<"((">>],<<"(">>,{1,6}},
                 joxa.compiler:'p-all'([FunC, FunS],
                                       <<"abc(((">>, index(),
                                       [])),
    ?assertMatch({fail,{expected,{'character-class',"abc"},{1,1}}},
                 joxa.compiler:'p-all'([FunC, FunS],
                                       <<"(abc">>, index(),
                                       [])),
    ?assertMatch({fail,{expected,{'character-class',"abc"},{1,1}}},
                 joxa.compiler:'p-all'([FunC, FunS],
                                       <<"xxxx">>, index(),
                                       none)).

p_seq_test() ->
    FunS = joxa.compiler:'p-string'(<<"((">>),
    FunC = joxa.compiler:'p-charclass'(<<"abc">>),
    FunT = joxa.compiler:'p-seq'([FunC, FunS]),
    ?assertMatch(true, erlang:is_function(FunS)),
    ?assertMatch(true, erlang:is_function(FunC)),
    ?assertMatch(true, erlang:is_function(FunT)),
    ?assertMatch({[<<"abc">>,<<"((">>],<<>>,{1,6}},
                 FunT(<<"abc((">>, index())),
    ?assertMatch({[<<"abc">>,<<"((">>],<<"(">>,{1,6}},
                 FunT(<<"abc(((">>, index())),
    ?assertMatch({fail,{expected,{'character-class',"abc"},{1,1}}},
                 FunT(<<"(abc">>, index())),
    ?assertMatch({fail,{expected,{'character-class',"abc"},{1,1}}},
                 FunT(<<"xxxx">>, index())).

p_and_test() ->
    FunS = joxa.compiler:'p-string'(<<"((">>),
    FunC = joxa.compiler:'p-charclass'(<<"abc">>),
    FunT = joxa.compiler:'p-and'([FunC, FunS]),
    ?assertMatch(true, erlang:is_function(FunS)),
    ?assertMatch(true, erlang:is_function(FunC)),
    ?assertMatch(true, erlang:is_function(FunT)),
    ?assertMatch({[<<"abc">>,<<"((">>],<<>>,{1,6}},
                 FunT(<<"abc((">>, index())),
    ?assertMatch({[<<"abc">>,<<"((">>],<<"(">>,{1,6}},
                 FunT(<<"abc(((">>, index())),
    ?assertMatch({fail,{expected,{'character-class',"abc"},{1,1}}},
                 FunT(<<"(abc">>, index())),
    ?assertMatch({fail,{expected,{'character-class',"abc"},{1,1}}},
                 FunT(<<"xxxx">>, index())).

p_assert_test() ->
    FunS = joxa.compiler:'p-string'("foo"),
    FunT = joxa.compiler:'p-assert'(FunS),
    ?assertMatch(true, erlang:is_function(FunS)),
    ?assertMatch(true, erlang:is_function(FunT)),
    ?assertMatch({[],<<"foo 123">>,{1,1}},
                 FunT(<<"foo 123">>, index())),
    ?assertMatch({fail,{expected,{string,<<"foo">>},{1,1}}},
                 FunT(<<"abc 123">>, index())),

    ?assertMatch({fail,{expected,{string,<<"foo">>},{1,1}}},
                 FunT(<<"12c 123">>, index())).

p_not_test() ->
    FunS = joxa.compiler:'p-string'("foo"),
    FunT = joxa.compiler:'p-not'(FunS),
    ?assertMatch(true, erlang:is_function(FunS)),
    ?assertMatch(true, erlang:is_function(FunT)),
    ?assertMatch({fail,{expected,{'no-match',<<"foo">>},{1,1}}},
                 FunT(<<"foo 123">>, index())),
    ?assertMatch({[],<<"abc 123">>,{1,1}},
                 FunT(<<"abc 123">>, index())),
    ?assertMatch({[],<<"12c 123">>,{1,1}},
                 FunT(<<"12c 123">>, index())).

p_optional_test() ->
    FunS = joxa.compiler:'p-string'("foo"),
    FunT = joxa.compiler:'p-optional'(FunS),
    ?assertMatch(true, erlang:is_function(FunS)),
    ?assertMatch(true, erlang:is_function(FunT)),
    ?assertMatch({<<"foo">>,<<" 123">>,{1,4}},
                 FunT(<<"foo 123">>, index())),
    ?assertMatch({[],<<"abc 123">>,{1,1}},
                 FunT(<<"abc 123">>, index())),
    ?assertMatch({[],<<"12c 123">>,{1,1}},
                 FunT(<<"12c 123">>, index())).

p_eof_test() ->
    FunT = joxa.compiler:'p-eof'(),
    ?assertMatch(true, erlang:is_function(FunT)),
    ?assertMatch({eof, <<>>, {1,1}},
                 FunT(<<>>, index())),
    ?assertMatch({fail, {expected, eof, {1,1}}},
                 FunT(<<"Foo">>, index())).

p_test() ->
    joxa.compiler:'setup-memo'(),
    FunS = joxa.compiler:'p-string'("foo"),
    FunT = fun(I, _) ->
                   I
           end,
    ?assertMatch(true, erlang:is_function(FunS)),
    ?assertMatch({<<"foo">>,<<>>,{1,4}},
                 joxa.compiler:p(<<"foo">>, index(),
                                 p_test, FunS, FunT)),
    %% This shouldn't match, but because the index is the same
    %% The memoized result comes back.
    ?assertMatch({<<"foo">>,<<>>,{1,4}},
                 joxa.compiler:p(<<"2">>, index(),
                                 p_test, FunS, FunT)),

    ?assertMatch({<<"foo">>,<<>>,{1,13}},
                 joxa.compiler:p(<<"foo">>, {1,10},
                                 p_test, FunS, FunT)),
    ?assertMatch({<<"foo">>,<<>>,{1,4}},
                 joxa.compiler:p(<<"still-ignored">>, index(),
                                 p_test, FunS, FunT)),
    ?assertMatch({<<"foo">>,<<>>,{1,13}},
                 joxa.compiler:p(<<"now-ignored">>, {1,10},
                                 p_test, FunS, FunT)),
    joxa.compiler:'release-memo'(),
    %% Now setup the memo again and things should fail.
    joxa.compiler:'setup-memo'(),
    ?assertMatch({fail,{expected,{string,<<"foo">>},{1,1}}},
                 joxa.compiler:p(<<"2">>, index(),
                                 p_test, FunS, FunT)),
    ?assertMatch({fail,{expected,{string,<<"foo">>},{1,10}}},
                 joxa.compiler:p(<<"now-ignored">>, {1,10},
                                 p_test, FunS, FunT)),
    joxa.compiler:'release-memo'().


p2_test() ->
    joxa.compiler:'setup-memo'(),
    FunS = joxa.compiler:'p-string'("foo"),
    ?assertMatch(true, erlang:is_function(FunS)),
    ?assertMatch({<<"foo">>,<<>>,{1,4}},
                 joxa.compiler:p(<<"foo">>, index(),
                                 p_test, FunS)),
    %% This shouldn't match, but because the index is the same
    %% The memoized result comes back.
    ?assertMatch({<<"foo">>,<<>>,{1,4}},
                 joxa.compiler:p(<<"2">>, index(),
                                 p_test, FunS)),

    ?assertMatch({<<"foo">>,<<>>,{1,13}},
                 joxa.compiler:p(<<"foo">>, {1,10},
                                 p_test, FunS)),
    ?assertMatch({<<"foo">>,<<>>,{1,4}},
                 joxa.compiler:p(<<"still-ignored">>, index(),
                                 p_test, FunS)),
    ?assertMatch({<<"foo">>,<<>>,{1,13}},
                 joxa.compiler:p(<<"now-ignored">>, {1,10},
                                 p_test, FunS)),
    joxa.compiler:'release-memo'(),
    %% Now setup the memo again and things should fail.
    joxa.compiler:'setup-memo'(),
    ?assertMatch({fail,{expected,{string,<<"foo">>},{1,1}}},
                 joxa.compiler:p(<<"2">>, index(),
                                 p_test, FunS)),
    ?assertMatch({fail,{expected,{string,<<"foo">>},{1,10}}},
                 joxa.compiler:p(<<"now-ignored">>, {1,10},
                                 p_test, FunS)),
    joxa.compiler:'release-memo'().

digit_test() ->
    ?memo(?assertMatch({fail,{expected,{'character-class',"[0-9]"},{1,1}}},
                       joxa.compiler:digit(<<"ab">>, index()))),
    ?memo(?assertMatch({<<"2">>,<<>>,{2,3}},
                       joxa.compiler:digit(<<"2">>, index(1)))),
    ?memo(?assertMatch({<<"2">>,<<"33">>,{3,4}},
                       joxa.compiler:digit(<<"233">>, index(2)))).

int_part_test() ->
    ?memo(?assertMatch({[[],[<<"2">>]],<<>>,{1,2}},
                       joxa.compiler:'int-part'(<<"2">>, index()))),
    ?memo(?assertMatch({[<<"-">>,[<<"2">>]],<<>>,{1,3}},
                       joxa.compiler:'int-part'(<<"-2">>, index()))).

frac_part_test() ->
    ?memo(?assertMatch({[<<".">>,[<<"2">>]],<<>>,{1,3}},
                       joxa.compiler:'frac-part'(<<".2">>, index()))),
    ?memo(?assertMatch({[<<".">>,[<<"2">>,<<"3">>,<<"3">>,<<"3">>]],
                        <<>>,
                        {2,7}},
                       joxa.compiler:'frac-part'(<<".2333">>, index(1)))),
    ?memo(?assertMatch({fail,{expected,{string,<<".">>},{3,3}}},
                       joxa.compiler:'frac-part'(<<"ee.2">>, index(2)))),
    ?memo(?assertMatch({fail,{expected,{string,<<".">>},{4,4}}},
                       joxa.compiler:'frac-part'(<<"eeff">>, index(3)))).

integer_test() ->
    ?memo(?assertMatch({{integer,123456,{1,1}},<<>>,{1,7}},
                       joxa.compiler:integer(<<"123456">>, index()))),
    ?memo(?assertMatch({{integer,1234,{1,1}},<<".56">>,{1,5}},
                       joxa.compiler:integer(<<"1234.56">>, index()))),
    ?memo(?assertMatch({fail,{expected,{'at-least-one',
                                        {'character-class',"[0-9]"}},{1,1}}},
                       joxa.compiler:integer(<<"abc123">>, index()))),
    ?memo(?assertMatch({{integer,123456,{1,1}},<<"abc">>,{1,7}},
                       joxa.compiler:integer(<<"123456abc">>, index()))).

float_test() ->
    ?memo(?assertMatch({{float,123456.22,{1,1}},<<>>,{1,10}},
                       joxa.compiler:float(<<"123456.22">>, index()))),
    ?memo(?assertMatch({{float,1234.56,{1,1}},<<>>,{1,8}},
                       joxa.compiler:float(<<"1234.56">>, index()))),
    ?memo(?assertMatch({fail,
                        {expected,{'at-least-one',
                                   {'character-class',"[0-9]"}},{1,1}}},
                       joxa.compiler:float(<<"abc123">>, index()))),
    ?memo(?assertMatch({fail,{expected,{string,<<".">>},{1,7}}},
                       joxa.compiler:float(<<"123456abc">>, index()))).

char_test() ->
    ?memo(?assertMatch({{char,97,{1,1}},<<>>,{1,3}},
                       joxa.compiler:char(<<"\\a">>, index()))),
    ?memo(?assertMatch({{char,123,{1,1}},<<>>,{1,3}},
                       joxa.compiler:char(<<"\\{">>, index()))),
    ?memo(?assertMatch({fail,{expected,{string,<<"\\">>},{1,1}}},
                       joxa.compiler:char(<<"ab">>, index()))),
    ?memo(?assertMatch({fail,{expected,{string,<<"\\">>},{1,1}}},
                       joxa.compiler:char(<<"(">>, index()))),
    ?memo(?assertMatch({{char,$",{1,1}},<<>>,{1,4}},
                       joxa.compiler:char(<<"\\\\\"">>, index()))),
    ?memo(?assertMatch({{char,$\b,{1,1}},<<>>,{1,4}},
                       joxa.compiler:char(<<"\\\\b">>, index()))),
    ?memo(?assertMatch({{char,$\f,{1,1}},<<>>,{1,4}},
                       joxa.compiler:char(<<"\\\\f">>, index()))),
    ?memo(?assertMatch({{char,$\n,{1,1}},<<>>,{1,4}},
                       joxa.compiler:char(<<"\\\\n">>, index()))),
    ?memo(?assertMatch({{char,$\r,{1,1}},<<>>,{1,4}},
                       joxa.compiler:char(<<"\\\\r">>, index()))),
    ?memo(?assertMatch({{char,$\t,{1,1}},<<>>,{1,4}},
                       joxa.compiler:char(<<"\\\\t">>, index()))).

space_test() ->
    ?memo(?assertMatch({<<" ">>,<<"  ">>,{1,2}},
                       joxa.compiler:space(<<"   ">>, index()))),
    ?memo(?assertMatch({<<"\t">>,<<" \n">>,{1,2}},
                       joxa.compiler:space(<<"\t \n">>, index()))),
    ?memo(?assertMatch({<<"\r">>,<<>>,{1,2}},
                       joxa.compiler:space(<<"\r">>, index()))),
    ?memo(?assertMatch({fail,{expected,
                              {'character-class',
                               "[ \t\n\\s\r]"},{1,1}}},
                       joxa.compiler:space(<<"abc">>, index()))).

comment_test() ->
    ?memo(?assertMatch({[<<";">>,
                         [<<";">>,<<";">>,<<" ">>,<<"h">>,<<"a">>,<<"h">>,<<"a">>],
                         eof],
                        <<>>,
                        {1,9}},
                       joxa.compiler:comment(<<";;; haha">>, index()))),
    ?memo(?assertMatch({[<<";">>,
                         [<<";">>,<<";">>,<<" ">>,<<"h">>,<<"a">> | _],
                         <<"\n">>],
                        <<>>,
                        {2,1}},
                       joxa.compiler:comment(<<";;; haha\n">>, index()))),
    ?memo(?assertMatch({[<<";">>,
                         [<<";">>,<<";">>,<<" ">>,<<"h">>,<<"a">> | _],
                         <<"\n">>],
                        <<"one">>,
                        {2,1}},
                       joxa.compiler:comment(<<";;; haha\none">>, index()))),
    ?memo(?assertMatch({[<<";">>,[],eof],<<>>,{1,2}},
                       joxa.compiler:comment(<<";">>, index()))),
    ?memo(?assertMatch({fail,{expected,{string,<<";">>},{1,1}}},
                       joxa.compiler:comment(<<"onetwothree">>, index()))).

ignorable_test() ->
    ?memo(?assertMatch({[],<<>>,{1,4}},
                       joxa.compiler:ignorable(<<"   ">>, index()))),
    ?memo(?assertMatch({[],<<>>,{2,1}},
                       joxa.compiler:ignorable(<<"\t \n">>, index()))),
    ?memo(?assertMatch({[],<<>>,{1,2}},
                       joxa.compiler:ignorable(<<"\r">>, index()))),
    ?memo(?assertMatch({[],<<"abc">>,{1,1}},
                       joxa.compiler:ignorable(<<"abc">>, index()))),
    ?memo(?assertMatch({[],<<>>,{1,9}},
                       joxa.compiler:ignorable(<<";;; haha">>, index()))),
    ?memo(?assertMatch({[],<<>>,{2,1}},
                       joxa.compiler:ignorable(<<";;; haha\n">>, index()))),
    ?memo(?assertMatch({[],<<"one">>,{2,1}},
                       joxa.compiler:ignorable(<<";;; haha\none">>, index()))),
    ?memo(?assertMatch({[],<<>>,{1,2}},
                       joxa.compiler:ignorable(<<";">>, index()))),
    ?memo(?assertMatch({[],<<"onetwothree">>,{1,1}},
                       joxa.compiler:ignorable(<<"onetwothree">>, index()))).


ident_test() ->
    ?memo(?assertMatch({{ident, 'true', {1, _}}, <<>>, _},
                       joxa.compiler:ident(<<"true">>, index()))),
    ?memo(?assertMatch({{ident, 'false', {1, _}},  <<>>, _},
                       joxa.compiler:ident(<<"false">>, index()))),
    ?memo(?assertMatch({{ident, '*foo*', {1, _}}, <<>>, _},
                       joxa.compiler:ident(<<"*foo*">>, index()))),
    ?memo(?assertMatch({{ident, 'foo-bar', {1, _}}, <<>>, _},
                       joxa.compiler:ident(<<"foo-bar">>, index()))),
    ?memo(?assertMatch({{ident, 'null', {1, _}}, <<>>, _},
                       joxa.compiler:ident(<<"null">>, index()))),
    ?memo(?assertMatch({{ident, 'Hello?', {1, _}}, <<>>, _},
                       joxa.compiler:ident(<<"Hello?">>, index()))),
    ?memo(?assertMatch({{ident, 'boo88', {1, _}}, <<>>, _},
                       joxa.compiler:ident(<<"boo88">>, index()))),
    ?memo(?assertMatch({{ident, 'bock:', {1, _}}, <<>>, _},
                       joxa.compiler:ident(<<"bock:">>, index()))),
    ?memo(?assertMatch({{ident, 'bock', {1, _}}, <<"{">>, _},
                       joxa.compiler:ident(<<"bock{">>, index()))),
    ?memo(?assertMatch({{ident, 'bock', {1, _}}, <<"[">>, _},
                       joxa.compiler:ident(<<"bock[">>, index()))),
    ?memo(?assertMatch({{ident, 'bock', {1, _}}, <<"(ee">>, _},
                       joxa.compiler:ident(<<"bock(ee">>, index()))).

symbol_test() ->
    ?memo(?assertMatch({{quote,{ident,true,{1,1}},{1,1}},<<>>,{1,6}},
                       joxa.compiler:symbol(<<":true">>, index()))),
    ?memo(?assertMatch({{quote,{ident,false,{1,1}},{1,1}},<<>>,{1,7}},
                       joxa.compiler:symbol(<<":false">>, index()))),
    ?memo(?assertMatch({{quote,{ident,'*foo*',{1,1}},{1,1}},
                        <<>>,
                        {1,7}},
                       joxa.compiler:symbol(<<":*foo*">>, index()))),
    ?memo(?assertMatch({fail,{expected,{string,<<":">>},{1,1}}},
                       joxa.compiler:symbol(<<"foo-bar">>, index()))),
    ?memo(?assertMatch({fail,{expected,{string,<<":">>},{1,1}}},
                       joxa.compiler:symbol(<<"null">>, index()))),
    ?memo(?assertMatch({{quote,{ident,'Hello?',{1,1}},{1,1}},
                        <<>>,
                        {1,8}},
                       joxa.compiler:symbol(<<":Hello?">>, index()))),
    ?memo(?assertMatch({{quote,{ident,boo88,{1,1}},{1,1}},<<>>,{1,7}},
                       joxa.compiler:symbol(<<":boo88">>, index()))),
    ?memo(?assertMatch({fail,{expected,{string,<<":">>},{1,1}}},
                       joxa.compiler:symbol(<<"bock:">>, index()))),
    ?memo(?assertMatch({fail,{expected,{string,<<":">>},{1,1}}},
                       joxa.compiler:symbol(<<"bock{">>, index()))),
    ?memo(?assertMatch({fail,{expected,{string,<<":">>},{1,1}}},
                       joxa.compiler:symbol(<<"bock[">>, index()))),
    ?memo(?assertMatch({{quote,{ident,bock,{1,1}},{1,1}},
                        <<"(ee">>,
                        {1,6}},
                       joxa.compiler:symbol(<<":bock(ee">>, index()))).


fun_reference_test() ->
    ?memo(?assertMatch({{call,{'__fun__','fun',3},{1,1}},<<>>,{1,6}},
                       joxa.compiler:'fun-reference'(<<"fun/3">>, index()))),
    ?memo(?assertMatch({{call,{'__fun__',module,'fun',3},{1,1}},
                        <<>>,
                        {1,13}},
                       joxa.compiler:'fun-reference'(<<"module/fun/3">>,
                                                     index()))),
    ?memo(?assertMatch({{call,{'__fun__',module,'fun'},{1,1}},
                        <<>>,
                        {1,11}},
                       joxa.compiler:'fun-reference'(<<"module/fun">>,
                                                     index()))),
    ?memo(?assertMatch({fail,{expected,{string,<<"/">>},{1,7}}},
                       joxa.compiler:'fun-reference'(<<"zoo_ma">>, index()))),
    ?memo(?assertMatch({fail,{expected,{'no-match',<<"/">>},{1,1}}},
                       joxa.compiler:'fun-reference'(<<"/2">>, index()))).


string_test() ->
    ?memo(?assertMatch({{string," \" ",{1,1}},<<>>,{1,7}},
                       joxa.compiler:string(<<"\" \\\" \"">>, index()))),

    ?memo(?assertMatch({{string,"\\",{1,1}},<<>>,{1,5}},
                       joxa.compiler:string(<<"\"\\\\\"">>,
                                            index()))),

    ?memo(?assertMatch({{string,"\f",{1,1}},<<>>,{1,5}},
                       joxa.compiler:string(<<"\"\\f\"">>,
                                            index()))),

    ?memo(?assertMatch({{string,"\t",{1,1}},<<>>,{1,5}},
                       joxa.compiler:string(<<"\"\\t\"">>,
                                            index()))),

    ?memo(?assertMatch({{string,"\n",{1,1}},<<>>,{1,5}},
                       joxa.compiler:string(<<"\"\\n\"">>, index()))),

    ?memo(?assertMatch({{string,"\r",{1,1}},<<>>,{1,5}},
                       joxa.compiler:string(<<"\"\\r\"">>, index()))).


quote_test() ->
    ?memo(?assertMatch({{quote,{ident,ok,{1,2}},{1,1}},<<>>,{1,4}},
                       joxa.compiler:quote(<<"'ok">>, index()))),

    ?memo(?assertMatch({{quote,{list,[],{1,2}},{1,1}},<<>>,{1,4}},
                       joxa.compiler:quote(<<"'()">>,
                                           index()))),

    ?memo(?assertMatch({{quote,{'literal-list',[],{1,2}},{1,1}},
                        <<>>,
                        {1,4}},
                       joxa.compiler:quote(<<"'[]">>,
                                           index()))),

    ?memo(?assertMatch({{quote,{integer,123,{1,2}},{1,1}},<<>>,{1,5}},
                       joxa.compiler:quote(<<"'123">>,
                                           index()))),

    ?memo(?assertMatch({{quote,{list,[{integer,1,_}],{1,2}},{1,1}},
                        <<>>,
                        {1,5}},
                       joxa.compiler:quote(<<"'(1)">>, index()))),

    ?memo(?assertMatch({{quote,{tuple,[{ident,one,_},{ident,two,_}],
                                {1,2}},{1,1}},
                        <<>>,
                        {1,11}},
                       joxa.compiler:quote(<<"'{one two}">>, index()))).

list_test() ->
    ?memo(?assertMatch({{list,[{quote,{ident,ok, _},{1,2}}],{1,1}},
                        <<>>,
                        {1,6}},
                       joxa.compiler:list(<<"(:ok)">>, index()))),

    ?memo(?assertMatch({{list,[{integer,1,{1,2}},
                               {integer,2,{1,4}},
                               {integer,3,_}],
                         {1,1}},
                        <<>>,
                        {1,8}},
                       joxa.compiler:list(<<"(1 2 3)">>,
                                          index()))),

    ?memo(?assertMatch({{'literal-list',
                         [{integer,33,{1,2}},
                          {quote,{ident,forty,_},{1,5}},
                          {list,[{integer,1, _},
                                 {integer, 2, _}],_},
                          {tuple,[{quote, {ident, hello, _}, _}],
                           _}],
                         {1,1}},
                        <<>>,
                        {1,27}},
                       joxa.compiler:list(<<"[33 :forty (1 2) {:hello}]">>,
                                          index()))),

    ?memo(?assertMatch({{list,
                         [{list,
                           [{list,[{list, [{integer, 123, _}], _},
                                   {integer, 1, _}], _},
                            {integer,2, _}],{1,2}},
                          {integer,3,{1,16}}],
                         {1,1}},
                        <<>>,
                        {1,18}},
                       joxa.compiler:list(<<"((((123) 1) 2) 3)">>,
                                          index()))),

    ?memo(?assertMatch({{list,[],{1,1}},<<>>,{1,3}},
                       joxa.compiler:list(<<"()">>, index()))),

    ?memo(?assertMatch({{list,[{list,[{list,[],_}],{1,2}}],{1,1}},
                        <<>>,
                        {1,7}},
                       joxa.compiler:list(<<"((()))">>, index()))).


tuple_test() ->
    ?memo(?assertMatch({{tuple,[{quote,{ident,ok, _},{1,2}}],{1,1}},
                        <<>>,
                        {1,6}},
                       joxa.compiler:tuple(<<"{:ok}">>, index()))),

    ?memo(?assertMatch({{tuple,[{integer,1,{1,2}},
                                {integer,2,{1,4}},
                                {integer,3,_}],
                         {1,1}},
                        <<>>,
                        {1,8}},
                       joxa.compiler:tuple(<<"{1 2 3}">>,
                                           index()))),

    ?memo(?assertMatch({{tuple,[{integer,33,{1,2}},
                                {quote,{ident,forty,_},{1,5}},
                                {list,[{integer, 1, _},
                                       {integer, 2, _}],_},
                                {tuple,[{quote, {ident, hello, _}, _}],
                                 _}],
                         {1,1}},
                        <<>>,
                        {1,27}},
                       joxa.compiler:tuple(<<"{33 :forty (1 2) {:hello}}">>,
                                           index()))),

    ?memo(?assertMatch({{tuple,
                         [{tuple,
                           [{tuple,
                             [{tuple, [{integer, 123, _}], _},
                              {integer, 1, _}], _},
                            {integer, 2, _}], _},
                          {integer,3,{1,16}}], {1,1}},
                        <<>>,
                        {1,18}},
                       joxa.compiler:tuple(<<"{{{{123} 1} 2} 3}">>,
                                           index()))),

    ?memo(?assertMatch({{tuple,[],{1,1}},<<>>,{1,3}},
                       joxa.compiler:tuple(<<"{}">>, index()))),

    ?memo(?assertMatch({{tuple,[{tuple,[{tuple,[], _}],{1,2}}],{1,1}},
                        <<>>,
                        {1,7}},
                       joxa.compiler:tuple(<<"{{{}}}">>, index()))).

binary_test() ->
    ?memo(?assertMatch({{binary,[{integer,1,{1,3}},
                                 {integer,2,{1,5}},
                                 {integer,3, _}],
                         {1,1}},
                        <<>>,
                        {1,10}},
                       joxa.compiler:binary(<<"<<1 2 3>>">>, index()))),

    ?memo(?assertMatch({{binary,[{char,97,{1,3}},
                                 {char,98,{1,6}},
                                 {char,99, _}],
                         {1,1}},
                        <<>>,
                        {1,13}},
                       joxa.compiler:binary(<<"<<\\a \\b \\c>>">>,
                                            index()))),

    ?memo(?assertMatch({{binary,[{ident,a,{1,3}},
                                 {ident,b,{1,5}},
                                 {list,[{ident, c, _},
                                        {quote, {ident, size, _}, _},
                                        {integer, 16, _}], _}],
                         {1,1}},
                        <<>>,
                        {1,21}},
                       joxa.compiler:binary(<<"<<a b (c :size 16)>>">>,
                                            index()))),

    ?memo(?assertMatch({{binary,
                         [{list,[{ident,d,_},
                                 {quote,{ident, size, _}, _},
                                 {integer, 16, _}],{1,3}},
                          {ident,e,{1,16}},
                          {list,[{ident, f, _},
                                 {quote, {ident, binary, _}, _}],
                           _}],
                         {1,1}},
                        <<>>,
                        _},
                       joxa.compiler:binary(<<"<<(d :size 16) e (f :binary)>>">>,
                                            index()))),

    ?memo(?assertMatch({{binary,[],{1,1}},<<>>,{1,5}},
                       joxa.compiler:binary(<<"<<>>">>, index()))),

    ?memo(?assertMatch({{binary,{string,[],{1,3}},{1,1}},
                        <<>>,
                        {1,7}},
                       joxa.compiler:binary(<<"<<\"\">>">>,
                                            index()))),
    ?memo(?assertMatch({{binary,{string,"HelloWorld",{1,4}},{1,1}},
                        <<>>,
                        {1,19}},
                       joxa.compiler:binary(<<"<< \"HelloWorld\" >>">>,
                                            index()))).

value_test() ->
    ?memo(?assertMatch({{ident, 'true', {1, _}}, <<>>, _},
                       joxa.compiler:value(<<"true">>, index()))),
    ?memo(?assertMatch({{ident, 'false', {1, _}},  <<>>, _},
                       joxa.compiler:value(<<"false">>, index()))),
    ?memo(?assertMatch({{ident, '*foo*', {1, _}}, <<>>, _},
                       joxa.compiler:value(<<"*foo*">>, index()))),
    ?memo(?assertMatch({{ident, 'foo-bar', {1, _}}, <<>>, _},
                       joxa.compiler:value(<<"foo-bar">>, index()))),
    ?memo(?assertMatch({{ident, 'null', {1, _}}, <<>>, _},
                       joxa.compiler:value(<<"null">>, index()))),
    ?memo(?assertMatch({{ident, 'Hello?', {1, _}}, <<>>, _},
                       joxa.compiler:value(<<"Hello?">>, index()))),
    ?memo(?assertMatch({{ident, 'boo88', {1, _}}, <<>>, _},
                       joxa.compiler:value(<<"boo88">>, index()))),
    ?memo(?assertMatch({{ident, 'bock:', {1, _}}, <<>>, _},
                       joxa.compiler:value(<<"bock:">>, index()))),
    ?memo(?assertMatch({{ident, 'bock', {1, _}}, <<"{">>, _},
                       joxa.compiler:value(<<"bock{">>, index()))),
    ?memo(?assertMatch({{ident, 'bock', {1, _}}, <<"[">>, _},
                       joxa.compiler:value(<<"bock[">>, index()))),
    ?memo(?assertMatch({{ident, 'bock', {1, _}}, <<"(ee">>, _},
                       joxa.compiler:value(<<"bock(ee">>, index()))),

    ?memo(?assertMatch({{quote,{ident,true,{1,1}},{1,1}},<<>>,{1,6}},
                       joxa.compiler:value(<<":true">>, index()))),
    ?memo(?assertMatch({{quote,{ident,false,{1,1}},{1,1}},<<>>,{1,7}},
                       joxa.compiler:value(<<":false">>, index()))),
    ?memo(?assertMatch({{quote,{ident,'*foo*',{1,1}},{1,1}},
                        <<>>,
                        {1,7}},
                       joxa.compiler:value(<<":*foo*">>, index()))),
    ?memo(?assertMatch({{ident,'foo-bar',{1,1}},<<>>,{1,8}},
                       joxa.compiler:value(<<"foo-bar">>, index()))),
    ?memo(?assertMatch({{quote,{ident,'Hello?',{1,1}},{1,1}},
                        <<>>,
                        {1,8}},
                       joxa.compiler:value(<<":Hello?">>, index()))),
    ?memo(?assertMatch({{quote,{ident,boo88,{1,1}},{1,1}},<<>>,{1,7}},
                       joxa.compiler:value(<<":boo88">>, index()))),
    ?memo(?assertMatch({{ident,'bock:',{1,1}},<<>>,{1,6}},
                       joxa.compiler:value(<<"bock:">>, index()))),
    ?memo(?assertMatch({{quote,{ident,bock,{1,1}},{1,1}},
                        <<"(ee">>,
                        {1,6}},
                       joxa.compiler:value(<<":bock(ee">>, index()))),

    ?memo(?assertMatch({{call,{'__fun__','fun',3},{1,1}},<<>>,{1,6}},
                       joxa.compiler:value(<<"fun/3">>, index()))),
    ?memo(?assertMatch({{call,{'__fun__',module,'fun',3},{1,1}},
                        <<>>,
                        {1,13}},
                       joxa.compiler:value(<<"module/fun/3">>,
                                           index()))),
    ?memo(?assertMatch({{call,{'__fun__',module,'fun'},{1,1}},
                        <<>>,
                        {1,11}},
                       joxa.compiler:value(<<"module/fun">>,
                                           index()))),
    ?memo(?assertMatch({fail,
                        {expected,{'at-least-one',
                                   {'character-class',"[0-9]"}},{1,1}}},
                       joxa.compiler:value(<<"/2">>, index()))),

    ?memo(?assertMatch({{string," \" ",{1,1}},<<>>,{1,7}},
                       joxa.compiler:value(<<"\" \\\" \"">>, index()))),

    ?memo(?assertMatch({{string,"\\",{1,1}},<<>>,{1,5}},
                       joxa.compiler:value(<<"\"\\\\\"">>,
                                           index()))),

    ?memo(?assertMatch({{string,"\f",{1,1}},<<>>,{1,5}},
                       joxa.compiler:value(<<"\"\\f\"">>,
                                           index()))),

    ?memo(?assertMatch({{string,"\t",{1,1}},<<>>,{1,5}},
                       joxa.compiler:value(<<"\"\\t\"">>,
                                           index()))),

    ?memo(?assertMatch({{string,"\n",{1,1}},<<>>,{1,5}},
                       joxa.compiler:value(<<"\"\\n\"">>, index()))),

    ?memo(?assertMatch({{string,"\r",{1,1}},<<>>,{1,5}},
                       joxa.compiler:value(<<"\"\\r\"">>, index()))),

    ?memo(?assertMatch({{quote,{ident,ok,{1,2}},{1,1}},<<>>,{1,4}},
                       joxa.compiler:value(<<"'ok">>, index()))),

    ?memo(?assertMatch({{quote,{list,[],{1,2}},{1,1}},<<>>,{1,4}},
                       joxa.compiler:value(<<"'()">>,
                                           index()))),

    ?memo(?assertMatch({{quote,{'literal-list',[],{1,2}},{1,1}},
                        <<>>,
                        {1,4}},
                       joxa.compiler:value(<<"'[]">>,
                                           index()))),

    ?memo(?assertMatch({{quote,{integer,123,{1,2}},{1,1}},<<>>,{1,5}},
                       joxa.compiler:value(<<"'123">>,
                                           index()))),

    ?memo(?assertMatch({{quote,{list,[{integer,1,_}],{1,2}},{1,1}},
                        <<>>,
                        {1,5}},
                       joxa.compiler:value(<<"'(1)">>, index()))),

    ?memo(?assertMatch({{quote,{tuple,[{ident,one,_},{ident,two,_}],
                                {1,2}},{1,1}},
                        <<>>,
                        {1,11}},
                       joxa.compiler:value(<<"'{one two}">>, index()))),

    ?memo(?assertMatch({{list,[{quote,{ident,ok, _},{1,2}}],{1,1}},
                        <<>>,
                        {1,6}},
                       joxa.compiler:value(<<"(:ok)">>, index()))),

    ?memo(?assertMatch({{list,[{integer,1,{1,2}},
                               {integer,2,{1,4}},
                               {integer,3,_}],
                         {1,1}},
                        <<>>,
                        {1,8}},
                       joxa.compiler:value(<<"(1 2 3)">>,
                                           index()))),

    ?memo(?assertMatch({{'literal-list',
                         [{integer,33,{1,2}},
                          {quote,{ident,forty,_},{1,5}},
                          {list,[{integer,1, _},
                                 {integer, 2, _}],_},
                          {tuple,[{quote, {ident, hello, _}, _}],
                           _}],
                         {1,1}},
                        <<>>,
                        {1,27}},
                       joxa.compiler:value(<<"[33 :forty (1 2) {:hello}]">>,
                                           index()))),

    ?memo(?assertMatch({{list,
                         [{list,
                           [{list,[{list, [{integer, 123, _}], _},
                                   {integer, 1, _}], _},
                            {integer,2, _}],{1,2}},
                          {integer,3,{1,16}}],
                         {1,1}},
                        <<>>,
                        {1,18}},
                       joxa.compiler:value(<<"((((123) 1) 2) 3)">>,
                                           index()))),

    ?memo(?assertMatch({{list,[],{1,1}},<<>>,{1,3}},
                       joxa.compiler:value(<<"()">>, index()))),

    ?memo(?assertMatch({{list,[{list,[{list,[],_}],{1,2}}],{1,1}},
                        <<>>,
                        {1,7}},
                       joxa.compiler:value(<<"((()))">>, index()))),

    ?memo(?assertMatch({{tuple,[{quote,{ident,ok, _},{1,2}}],{1,1}},
                        <<>>,
                        {1,6}},
                       joxa.compiler:value(<<"{:ok}">>, index()))),

    ?memo(?assertMatch({{tuple,[{integer,1,{1,2}},
                                {integer,2,{1,4}},
                                {integer,3,_}],
                         {1,1}},
                        <<>>,
                        {1,8}},
                       joxa.compiler:value(<<"{1 2 3}">>,
                                           index()))),

    ?memo(?assertMatch({{tuple,[{integer,33,{1,2}},
                                {quote,{ident,forty,_},{1,5}},
                                {list,[{integer, 1, _},
                                       {integer, 2, _}],_},
                                {tuple,[{quote, {ident, hello, _}, _}],
                                 _}],
                         {1,1}},
                        <<>>,
                        {1,27}},
                       joxa.compiler:value(<<"{33 :forty (1 2) {:hello}}">>,
                                           index()))),

    ?memo(?assertMatch({{tuple,
                         [{tuple,
                           [{tuple,
                             [{tuple, [{integer, 123, _}], _},
                              {integer, 1, _}], _},
                            {integer, 2, _}], _},
                          {integer,3,{1,16}}], {1,1}},
                        <<>>,
                        {1,18}},
                       joxa.compiler:value(<<"{{{{123} 1} 2} 3}">>,
                                           index()))),

    ?memo(?assertMatch({{tuple,[],{1,1}},<<>>,{1,3}},
                       joxa.compiler:value(<<"{}">>, index()))),

    ?memo(?assertMatch({{tuple,[{tuple,[{tuple,[], _}],{1,2}}],{1,1}},
                        <<>>,
                        {1,7}},
                       joxa.compiler:value(<<"{{{}}}">>, index()))),

    ?memo(?assertMatch({{binary,[{integer,1,{1,3}},
                                 {integer,2,{1,5}},
                                 {integer,3, _}],
                         {1,1}},
                        <<>>,
                        {1,10}},
                       joxa.compiler:value(<<"<<1 2 3>>">>, index()))),

    ?memo(?assertMatch({{binary,[{char,97,{1,3}},
                                 {char,98,{1,6}},
                                 {char,99, _}],
                         {1,1}},
                        <<>>,
                        {1,13}},
                       joxa.compiler:value(<<"<<\\a \\b \\c>>">>,
                                           index()))),

    ?memo(?assertMatch({{binary,[{ident,a,{1,3}},
                                 {ident,b,{1,5}},
                                 {list,[{ident, c, _},
                                        {quote, {ident, size, _}, _},
                                        {integer, 16, _}], _}],
                         {1,1}},
                        <<>>,
                        {1,21}},
                       joxa.compiler:value(<<"<<a b (c :size 16)>>">>,
                                           index()))),

    ?memo(?assertMatch({{binary,
                         [{list,[{ident,d,_},
                                 {quote,{ident, size, _}, _},
                                 {integer, 16, _}],{1,3}},
                          {ident,e,{1,16}},
                          {list,[{ident, f, _},
                                 {quote, {ident, binary, _}, _}],
                           _}],
                         {1,1}},
                        <<>>,
                        {1,31}},
                       joxa.compiler:value(<<"<<(d :size 16) e (f :binary)>>">>,
                                           index()))),

    ?memo(?assertMatch({{binary,[],{1,1}},<<>>,{1,5}},
                       joxa.compiler:value(<<"<<>>">>, index()))),

    ?memo(?assertMatch({{binary,{string,"",{1,3}},{1,1}},
                        <<>>,
                        {1,7}},
                       joxa.compiler:value(<<"<<\"\">>">>,
                                           index()))),
    ?memo(?assertMatch({{binary,{string,"HelloWorld",{1,4}},{1,1}},
                        <<>>,
                        {1,19}},
                       joxa.compiler:value(<<"<< \"HelloWorld\" >>">>,
                                           index()))).



%%%===================================================================
%%% Support Functions
%%%===================================================================
index() ->
    {1, 1}.

index(N) ->
    {1+N, 1+N}.
