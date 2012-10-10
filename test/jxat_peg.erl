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
        'joxa-compiler':'setup-memo'(),
        X,
        'joxa-compiler':'release-memo'()).

%%%===================================================================
%%% Tests
%%%===================================================================
line_test() ->
    ?assertMatch(100, 'joxa-cmp-peg':line({100, 0})),
    ?assertError({case_clause,foo}, 'joxa-cmp-peg':line(foo)).

column_test() ->

    ?assertMatch(56, 'joxa-cmp-peg':column({100, 56})),
    ?assertError({case_clause,foo}, 'joxa-cmp-peg':column(foo)).

p_charclass_test() ->
    Fun = 'joxa-cmp-peg':'p-charclass'("abc"),
    ?assertMatch(true, erlang:is_function(Fun)),
    ?assertMatch({<<"abc">>,<<"123">>,{1, 4}},
                 Fun(<<"abc123">>, index())),
    ?assertMatch({fail,{expected,{'character-class',"abc"},{1,1}}},
                 Fun(<<"123">>, index())),
    ?assertMatch({<<"abc">>,<<"\n123">>,{1,4}},
                 Fun(<<"abc\n123">>, index())).

p_anything_test() ->
    Fun = 'joxa-cmp-peg':'p-anything'(),
    ?assertMatch(true, erlang:is_function(Fun)),
    ?assertMatch({97,<<"bc123">>,{1, 2}},
                 Fun(<<"abc123">>, index())),
    ?assertMatch({fail,{expected, 'any-character', {1,1}}},
                 Fun(<<>>, index())),
    ?assertMatch({97,<<"bc\n123">>,{1,2}},
                 Fun(<<"abc\n123">>, index())).
p_string_test() ->
    FunS = 'joxa-cmp-peg':'p-string'("foo"),
    FunB = 'joxa-cmp-peg':'p-string'(<<"foo">>),

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
    FunS = 'joxa-cmp-peg':'p-string'("foo"),
    ?assertMatch(true, erlang:is_function(FunS)),
    ?assertMatch({[<<"foo">>],<<" 123">>,{1,4}},
                 'joxa-cmp-peg':'p-scan'(FunS,
                                          <<"foo 123">>,
                                          index(),
                                          [])),
    ?assertMatch({[],<<"abc 123">>,{1,1}},
                 'joxa-cmp-peg':'p-scan'(FunS,
                                          <<"abc 123">>,
                                          index(),
                                          [])),
    ?assertMatch({[],<<"12c 123">>,{1,1}},
                 'joxa-cmp-peg':'p-scan'(FunS,
                                          <<"12c 123">>,
                                          index(),
                                          [])).
p_one_or_more_test() ->
    FunS = 'joxa-cmp-peg':'p-string'("("),
    FunT = 'joxa-cmp-peg':'p-one-or-more'(FunS),
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
    FunS = 'joxa-cmp-peg':'p-string'("foo"),
    FunT = 'joxa-cmp-peg':'p-zero-or-more'(FunS),
    ?assertMatch(true, erlang:is_function(FunS)),
    ?assertMatch(true, erlang:is_function(FunT)),
    ?assertMatch({[<<"foo">>],<<" 123">>,{1,4}},
                 FunT(<<"foo 123">>, index())),
    ?assertMatch({[],<<"abc 123">>,{1,1}},
                 FunT(<<"abc 123">>, index())),

    ?assertMatch({[],<<"12c 123">>,{1,1}},
                 FunT(<<"12c 123">>, index())).


p_attempt_test() ->
    FunS = 'joxa-cmp-peg':'p-string'(<<"(">>),
    FunC = 'joxa-cmp-peg':'p-charclass'(<<"abc">>),
    ?assertMatch(true, erlang:is_function(FunS)),
    ?assertMatch(true, erlang:is_function(FunC)),
    ?assertMatch({<<"abc">>,<<"(((">>,{1,4}},
                 'joxa-cmp-peg':'p-attempt'([FunS, FunC],
                                             <<"abc(((">>, index(),
                                             none)),
    ?assertMatch({<<"(">>,<<"((abc">>,{1,2}},
                 'joxa-cmp-peg':'p-attempt'([FunS, FunC],
                                             <<"(((abc">>, index(),
                                             none)),
    ?assertMatch({fail,{expected,{string,<<"(">>},{1,1}}},
                 'joxa-cmp-peg':'p-attempt'([FunS, FunC],
                                             <<"xxxx">>, index(),
                                             none)).

p_choose_test() ->
    FunS = 'joxa-cmp-peg':'p-string'(<<"(">>),
    FunC = 'joxa-cmp-peg':'p-charclass'(<<"abc">>),
    FunT = 'joxa-cmp-peg':'p-choose'([FunS, FunC]),
    ?assertMatch(true, erlang:is_function(FunS)),
    ?assertMatch(true, erlang:is_function(FunC)),
    ?assertMatch({<<"abc">>,<<"(((">>,{1,4}},
                 FunT(<<"abc(((">>, index())),
    ?assertMatch({<<"(">>,<<"((abc">>,{1,2}},
                 FunT(<<"(((abc">>, index())),
    ?assertMatch({fail,{expected,{string,<<"(">>},{1,1}}},
                 FunT(<<"xxxx">>, index())).


p_all_test() ->
    FunS = 'joxa-cmp-peg':'p-string'(<<"((">>),
    FunC = 'joxa-cmp-peg':'p-charclass'(<<"abc">>),
    ?assertMatch(true, erlang:is_function(FunS)),
    ?assertMatch(true, erlang:is_function(FunC)),
    ?assertMatch({[<<"abc">>,<<"((">>],<<>>,{1,6}},
                 'joxa-cmp-peg':'p-all'([FunC, FunS],
                                         <<"abc((">>, index(),
                                         [])),
    ?assertMatch({[<<"abc">>,<<"((">>],<<"(">>,{1,6}},
                 'joxa-cmp-peg':'p-all'([FunC, FunS],
                                         <<"abc(((">>, index(),
                                         [])),
    ?assertMatch({fail,{expected,{'character-class',"abc"},{1,1}}},
                 'joxa-cmp-peg':'p-all'([FunC, FunS],
                                         <<"(abc">>, index(),
                                         [])),
    ?assertMatch({fail,{expected,{'character-class',"abc"},{1,1}}},
                 'joxa-cmp-peg':'p-all'([FunC, FunS],
                                         <<"xxxx">>, index(),
                                         none)).

p_seq_test() ->
    FunS = 'joxa-cmp-peg':'p-string'(<<"((">>),
    FunC = 'joxa-cmp-peg':'p-charclass'(<<"abc">>),
    FunT = 'joxa-cmp-peg':'p-seq'([FunC, FunS]),
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
    FunS = 'joxa-cmp-peg':'p-string'(<<"((">>),
    FunC = 'joxa-cmp-peg':'p-charclass'(<<"abc">>),
    FunT = 'joxa-cmp-peg':'p-and'([FunC, FunS]),
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
    FunS = 'joxa-cmp-peg':'p-string'("foo"),
    FunT = 'joxa-cmp-peg':'p-assert'(FunS),
    ?assertMatch(true, erlang:is_function(FunS)),
    ?assertMatch(true, erlang:is_function(FunT)),
    ?assertMatch({[],<<"foo 123">>,{1,1}},
                 FunT(<<"foo 123">>, index())),
    ?assertMatch({fail,{expected,{string,<<"foo">>},{1,1}}},
                 FunT(<<"abc 123">>, index())),

    ?assertMatch({fail,{expected,{string,<<"foo">>},{1,1}}},
                 FunT(<<"12c 123">>, index())).

p_not_test() ->
    FunS = 'joxa-cmp-peg':'p-string'("foo"),
    FunT = 'joxa-cmp-peg':'p-not'(FunS),
    ?assertMatch(true, erlang:is_function(FunS)),
    ?assertMatch(true, erlang:is_function(FunT)),
    ?assertMatch({fail,{expected,{'no-match',<<"foo">>},{1,1}}},
                 FunT(<<"foo 123">>, index())),
    ?assertMatch({[],<<"abc 123">>,{1,1}},
                 FunT(<<"abc 123">>, index())),
    ?assertMatch({[],<<"12c 123">>,{1,1}},
                 FunT(<<"12c 123">>, index())).

p_optional_test() ->
    FunS = 'joxa-cmp-peg':'p-string'("foo"),
    FunT = 'joxa-cmp-peg':'p-optional'(FunS),
    ?assertMatch(true, erlang:is_function(FunS)),
    ?assertMatch(true, erlang:is_function(FunT)),
    ?assertMatch({<<"foo">>,<<" 123">>,{1,4}},
                 FunT(<<"foo 123">>, index())),
    ?assertMatch({[],<<"abc 123">>,{1,1}},
                 FunT(<<"abc 123">>, index())),
    ?assertMatch({[],<<"12c 123">>,{1,1}},
                 FunT(<<"12c 123">>, index())).

p_eof_test() ->
    FunT = 'joxa-cmp-peg':'p-eof'(),
    ?assertMatch(true, erlang:is_function(FunT)),
    ?assertMatch({eof, <<>>, {1,1}},
                 FunT(<<>>, index())),
    ?assertMatch({fail, {expected, eof, {1,1}}},
                 FunT(<<"Foo">>, index())).

p_test() ->
    'joxa-cmp-peg':'setup-memo'(),
    FunS = 'joxa-cmp-peg':'p-string'("foo"),
    FunT = fun(I, _) ->
                   I
           end,
    ?assertMatch(true, erlang:is_function(FunS)),
    ?assertMatch({<<"foo">>,<<>>,{1,4}},
                 'joxa-cmp-peg':p(<<"foo">>, index(),
                                   p_test, FunS, FunT)),
    %% This shouldn't match, but because the index is the same
    %% The memoized result comes back.
    ?assertMatch({<<"foo">>,<<>>,{1,4}},
                 'joxa-cmp-peg':p(<<"2">>, index(),
                                   p_test, FunS, FunT)),

    ?assertMatch({<<"foo">>,<<>>,{1,13}},
                 'joxa-cmp-peg':p(<<"foo">>, {1,10},
                                   p_test, FunS, FunT)),
    ?assertMatch({<<"foo">>,<<>>,{1,4}},
                 'joxa-cmp-peg':p(<<"still-ignored">>, index(),
                                   p_test, FunS, FunT)),
    ?assertMatch({<<"foo">>,<<>>,{1,13}},
                 'joxa-cmp-peg':p(<<"now-ignored">>, {1,10},
                                   p_test, FunS, FunT)),
    'joxa-cmp-peg':'release-memo'(),
    %% Now setup the memo again and things should fail.
    'joxa-cmp-peg':'setup-memo'(),
    ?assertMatch({fail,{expected,{string,<<"foo">>},{1,1}}},
                 'joxa-cmp-peg':p(<<"2">>, index(),
                                   p_test, FunS, FunT)),
    ?assertMatch({fail,{expected,{string,<<"foo">>},{1,10}}},
                 'joxa-cmp-peg':p(<<"now-ignored">>, {1,10},
                                   p_test, FunS, FunT)),
    'joxa-cmp-peg':'release-memo'().


p2_test() ->
    'joxa-cmp-peg':'setup-memo'(),
    FunS = 'joxa-cmp-peg':'p-string'("foo"),
    ?assertMatch(true, erlang:is_function(FunS)),
    ?assertMatch({<<"foo">>,<<>>,{1,4}},
                 'joxa-cmp-peg':p(<<"foo">>, index(),
                                   p_test, FunS)),
    %% This shouldn't match, but because the index is the same
    %% The memoized result comes back.
    ?assertMatch({<<"foo">>,<<>>,{1,4}},
                 'joxa-cmp-peg':p(<<"2">>, index(),
                                   p_test, FunS)),

    ?assertMatch({<<"foo">>,<<>>,{1,13}},
                 'joxa-cmp-peg':p(<<"foo">>, {1,10},
                                   p_test, FunS)),
    ?assertMatch({<<"foo">>,<<>>,{1,4}},
                 'joxa-cmp-peg':p(<<"still-ignored">>, index(),
                                   p_test, FunS)),
    ?assertMatch({<<"foo">>,<<>>,{1,13}},
                 'joxa-cmp-peg':p(<<"now-ignored">>, {1,10},
                                   p_test, FunS)),
    'joxa-cmp-peg':'release-memo'(),
    %% Now setup the memo again and things should fail.
    'joxa-cmp-peg':'setup-memo'(),
    ?assertMatch({fail,{expected,{string,<<"foo">>},{1,1}}},
                 'joxa-cmp-peg':p(<<"2">>, index(),
                                   p_test, FunS)),
    ?assertMatch({fail,{expected,{string,<<"foo">>},{1,10}}},
                 'joxa-cmp-peg':p(<<"now-ignored">>, {1,10},
                                   p_test, FunS)),
    'joxa-cmp-peg':'release-memo'().


%%%===================================================================
%%% Support Functions
%%%===================================================================
index() ->
    {1, 1}.
