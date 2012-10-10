%%%-------------------------------------------------------------------
%%% @author Eric B Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2011, Eric B Merritt
%%% @doc
%%%
%%% @end
%%% Created : 29 Dec 2011 by Eric B Merritt <ericbmerritt@gmail.com>
%%%-------------------------------------------------------------------
-module(jxat_lexer).

%% API
-include_lib("eunit/include/eunit.hrl").

-define(memo(X),
        'joxa-cmp-peg':'setup-memo'(),
        X,
        'joxa-cmp-peg':'release-memo'()).

%%%===================================================================
%%% Tests
%%%===================================================================

digit_test() ->
    ?memo(?assertMatch({fail,{expected,{'character-class',"[0-9]"},{1,1}}},
                       'joxa-cmp-lexer':digit(<<"ab">>, index()))),
    ?memo(?assertMatch({<<"2">>,<<>>,{2,3}},
                       'joxa-cmp-lexer':digit(<<"2">>, index(1)))),
    ?memo(?assertMatch({<<"2">>,<<"33">>,{3,4}},
                       'joxa-cmp-lexer':digit(<<"233">>, index(2)))).

int_part_test() ->
    ?memo(?assertMatch({[[],[<<"2">>]],<<>>,{1,2}},
                       'joxa-cmp-lexer':'int-part'(<<"2">>, index()))),
    ?memo(?assertMatch({[<<"-">>,[<<"2">>]],<<>>,{1,3}},
                       'joxa-cmp-lexer':'int-part'(<<"-2">>, index()))).

frac_part_test() ->
    ?memo(?assertMatch({[<<".">>,[<<"2">>]],<<>>,{1,3}},
                       'joxa-cmp-lexer':'frac-part'(<<".2">>, index()))),
    ?memo(?assertMatch({[<<".">>,[<<"2">>,<<"3">>,<<"3">>,<<"3">>]],
                        <<>>,
                        {2,7}},
                       'joxa-cmp-lexer':'frac-part'(<<".2333">>, index(1)))),
    ?memo(?assertMatch({fail,{expected,{string,<<".">>},{3,3}}},
                       'joxa-cmp-lexer':'frac-part'(<<"ee.2">>, index(2)))),
    ?memo(?assertMatch({fail,{expected,{string,<<".">>},{4,4}}},
                       'joxa-cmp-lexer':'frac-part'(<<"eeff">>, index(3)))).

integer_test() ->
    ?memo(?assertMatch({{integer,123456,{1,1}},<<>>,{1,7}},
                       'joxa-cmp-lexer':integer(<<"123456">>, index()))),
    ?memo(?assertMatch({{integer,1234,{1,1}},<<".56">>,{1,5}},
                       'joxa-cmp-lexer':integer(<<"1234.56">>, index()))),
    ?memo(?assertMatch({fail,{expected,{'at-least-one',
                                        {'character-class',"[0-9]"}},{1,1}}},
                       'joxa-cmp-lexer':integer(<<"abc123">>, index()))),
    ?memo(?assertMatch({{integer,123456,{1,1}},<<"abc">>,{1,7}},
                       'joxa-cmp-lexer':integer(<<"123456abc">>, index()))).

float_test() ->
    ?memo(?assertMatch({{float,123456.22,{1,1}},<<>>,{1,10}},
                       'joxa-cmp-lexer':float(<<"123456.22">>, index()))),
    ?memo(?assertMatch({{float,1234.56,{1,1}},<<>>,{1,8}},
                       'joxa-cmp-lexer':float(<<"1234.56">>, index()))),
    ?memo(?assertMatch({fail,
                        {expected,{'at-least-one',
                                   {'character-class',"[0-9]"}},{1,1}}},
                       'joxa-cmp-lexer':float(<<"abc123">>, index()))),
    ?memo(?assertMatch({fail,{expected,{string,<<".">>},{1,7}}},
                       'joxa-cmp-lexer':float(<<"123456abc">>, index()))).

char_test() ->
    ?memo(?assertMatch({{char,97,{1,1}},<<>>,{1,3}},
                       'joxa-cmp-lexer':char(<<"\\a">>, index()))),
    ?memo(?assertMatch({{char,123,{1,1}},<<>>,{1,3}},
                       'joxa-cmp-lexer':char(<<"\\{">>, index()))),
    ?memo(?assertMatch({fail,{expected,{string,<<"\\">>},{1,1}}},
                       'joxa-cmp-lexer':char(<<"ab">>, index()))),
    ?memo(?assertMatch({fail,{expected,{string,<<"\\">>},{1,1}}},
                       'joxa-cmp-lexer':char(<<"(">>, index()))),
    ?memo(?assertMatch({{char,$",{1,1}},<<>>,{1,4}},
                       'joxa-cmp-lexer':char(<<"\\\\\"">>, index()))),
    ?memo(?assertMatch({{char,$\b,{1,1}},<<>>,{1,4}},
                       'joxa-cmp-lexer':char(<<"\\\\b">>, index()))),
    ?memo(?assertMatch({{char,$\f,{1,1}},<<>>,{1,4}},
                       'joxa-cmp-lexer':char(<<"\\\\f">>, index()))),
    ?memo(?assertMatch({{char,$\n,{1,1}},<<>>,{1,4}},
                       'joxa-cmp-lexer':char(<<"\\\\n">>, index()))),
    ?memo(?assertMatch({{char,$\r,{1,1}},<<>>,{1,4}},
                       'joxa-cmp-lexer':char(<<"\\\\r">>, index()))),
    ?memo(?assertMatch({{char,$\t,{1,1}},<<>>,{1,4}},
                       'joxa-cmp-lexer':char(<<"\\\\t">>, index()))).

space_test() ->
    ?memo(?assertMatch({<<" ">>,<<"  ">>,{1,2}},
                       'joxa-cmp-lexer':space(<<"   ">>, index()))),
    ?memo(?assertMatch({<<"\t">>,<<" \n">>,{1,2}},
                       'joxa-cmp-lexer':space(<<"\t \n">>, index()))),
    ?memo(?assertMatch({<<"\r">>,<<>>,{1,2}},
                       'joxa-cmp-lexer':space(<<"\r">>, index()))),
    ?memo(?assertMatch({fail,{expected,
                              {'character-class',
                               "[ \t\n\\s\r]"},{1,1}}},
                       'joxa-cmp-lexer':space(<<"abc">>, index()))).

comment_test() ->
    ?memo(?assertMatch({[<<";">>,
                         [<<";">>,<<";">>,<<" ">>,<<"h">>,<<"a">>,<<"h">>,<<"a">>],
                         eof],
                        <<>>,
                        {1,9}},
                       'joxa-cmp-lexer':comment(<<";;; haha">>, index()))),
    ?memo(?assertMatch({[<<";">>,
                         [<<";">>,<<";">>,<<" ">>,<<"h">>,<<"a">> | _],
                         <<"\n">>],
                        <<>>,
                        {2,1}},
                       'joxa-cmp-lexer':comment(<<";;; haha\n">>, index()))),
    ?memo(?assertMatch({[<<";">>,
                         [<<";">>,<<";">>,<<" ">>,<<"h">>,<<"a">> | _],
                         <<"\n">>],
                        <<"one">>,
                        {2,1}},
                       'joxa-cmp-lexer':comment(<<";;; haha\none">>, index()))),
    ?memo(?assertMatch({[<<";">>,[],eof],<<>>,{1,2}},
                       'joxa-cmp-lexer':comment(<<";">>, index()))),
    ?memo(?assertMatch({fail,{expected,{string,<<";">>},{1,1}}},
                       'joxa-cmp-lexer':comment(<<"onetwothree">>, index()))).

ignorable_test() ->
    ?memo(?assertMatch({[],<<>>,{1,4}},
                       'joxa-cmp-lexer':ignorable(<<"   ">>, index()))),
    ?memo(?assertMatch({[],<<>>,{2,1}},
                       'joxa-cmp-lexer':ignorable(<<"\t \n">>, index()))),
    ?memo(?assertMatch({[],<<>>,{1,2}},
                       'joxa-cmp-lexer':ignorable(<<"\r">>, index()))),
    ?memo(?assertMatch({[],<<"abc">>,{1,1}},
                       'joxa-cmp-lexer':ignorable(<<"abc">>, index()))),
    ?memo(?assertMatch({[],<<>>,{1,9}},
                       'joxa-cmp-lexer':ignorable(<<";;; haha">>, index()))),
    ?memo(?assertMatch({[],<<>>,{2,1}},
                       'joxa-cmp-lexer':ignorable(<<";;; haha\n">>, index()))),
    ?memo(?assertMatch({[],<<"one">>,{2,1}},
                       'joxa-cmp-lexer':ignorable(<<";;; haha\none">>, index()))),
    ?memo(?assertMatch({[],<<>>,{1,2}},
                       'joxa-cmp-lexer':ignorable(<<";">>, index()))),
    ?memo(?assertMatch({[],<<"onetwothree">>,{1,1}},
                       'joxa-cmp-lexer':ignorable(<<"onetwothree">>, index()))).


ident_test() ->
    ?memo(?assertMatch({{ident, 'true', {1, _}}, <<>>, _},
                       'joxa-cmp-lexer':ident(<<"true">>, index()))),
    ?memo(?assertMatch({{ident, 'false', {1, _}},  <<>>, _},
                       'joxa-cmp-lexer':ident(<<"false">>, index()))),
    ?memo(?assertMatch({{ident, '*foo*', {1, _}}, <<>>, _},
                       'joxa-cmp-lexer':ident(<<"*foo*">>, index()))),
    ?memo(?assertMatch({{ident, 'foo-bar', {1, _}}, <<>>, _},
                       'joxa-cmp-lexer':ident(<<"foo-bar">>, index()))),
    ?memo(?assertMatch({{ident, 'null', {1, _}}, <<>>, _},
                       'joxa-cmp-lexer':ident(<<"null">>, index()))),
    ?memo(?assertMatch({{ident, 'Hello?', {1, _}}, <<>>, _},
                       'joxa-cmp-lexer':ident(<<"Hello?">>, index()))),
    ?memo(?assertMatch({{ident, 'boo88', {1, _}}, <<>>, _},
                       'joxa-cmp-lexer':ident(<<"boo88">>, index()))),
    ?memo(?assertMatch({{ident, 'bock:', {1, _}}, <<>>, _},
                       'joxa-cmp-lexer':ident(<<"bock:">>, index()))),
    ?memo(?assertMatch({{ident, 'bock', {1, _}}, <<"{">>, _},
                       'joxa-cmp-lexer':ident(<<"bock{">>, index()))),
    ?memo(?assertMatch({{ident, 'bock', {1, _}}, <<"[">>, _},
                       'joxa-cmp-lexer':ident(<<"bock[">>, index()))),
    ?memo(?assertMatch({{ident, 'bock', {1, _}}, <<"(ee">>, _},
                       'joxa-cmp-lexer':ident(<<"bock(ee">>, index()))).

fun_reference_test() ->
    ?memo(?assertMatch({{call,{'--fun','fun',3},{1,1}},<<>>,{1,6}},
                       'joxa-cmp-lexer':'fun-reference'(<<"fun/3">>, index()))),
    ?memo(?assertMatch({{call,{'--fun',module,'fun',3},{1,1}},
                        <<>>,
                        {1,13}},
                       'joxa-cmp-lexer':'fun-reference'(<<"module/fun/3">>,
                                                       index()))),
    ?memo(?assertMatch({{call,{'--fun',module,'fun'},{1,1}},
                        <<>>,
                        {1,11}},
                       'joxa-cmp-lexer':'fun-reference'(<<"module/fun">>,
                                                       index()))),
    ?memo(?assertMatch({fail,{expected,{string,<<"/">>},{1,7}}},
                       'joxa-cmp-lexer':'fun-reference'(<<"zoo_ma">>, index()))),
    ?memo(?assertMatch({fail,{expected,{string,<<":'">>},{1,1}}},
                       'joxa-cmp-lexer':'fun-reference'(<<"/2">>, index()))).


string_test() ->
    ?memo(?assertMatch({{string," \" ",{1,1}},<<>>,{1,7}},
                       'joxa-cmp-lexer':string(<<"\" \\\" \"">>, index()))),

    ?memo(?assertMatch({{string,"\\",{1,1}},<<>>,{1,5}},
                       'joxa-cmp-lexer':string(<<"\"\\\\\"">>,
                                              index()))),

    ?memo(?assertMatch({{string,"\f",{1,1}},<<>>,{1,5}},
                       'joxa-cmp-lexer':string(<<"\"\\f\"">>,
                                              index()))),

    ?memo(?assertMatch({{string,"\t",{1,1}},<<>>,{1,5}},
                       'joxa-cmp-lexer':string(<<"\"\\t\"">>,
                                              index()))),

    ?memo(?assertMatch({{string,"\n",{1,1}},<<>>,{1,5}},
                       'joxa-cmp-lexer':string(<<"\"\\n\"">>, index()))),

    ?memo(?assertMatch({{string,"\r",{1,1}},<<>>,{1,5}},
                       'joxa-cmp-lexer':string(<<"\"\\r\"">>, index()))).


quoted_ident_test() ->
    ?memo(?assertMatch({{ident, ok, _},<<>>,_},
                       'joxa-cmp-lexer':'quoted-ident'(<<":'ok'">>, index()))),

    ?memo(?assertMatch({{ident, '()', _},<<>>,_},
                       'joxa-cmp-lexer':'quoted-ident'(<<":'()'">>,
                                                      index()))),

    ?memo(?assertMatch({{ident, '[]', _},
                        <<>>,
                        _},
                       'joxa-cmp-lexer':'quoted-ident'(<<":'[]'">>,
                                                      index()))),

    ?memo(?assertMatch({{ident, '123', _},<<>>,_},
                       'joxa-cmp-lexer':'quoted-ident'(<<":'123'">>,
                                                      index()))),

    ?memo(?assertMatch({{ident, '(1)', _},
                        <<>>,
                        _},
                       'joxa-cmp-lexer':'quoted-ident'(<<":'(1)'">>, index()))),

    ?memo(?assertMatch({{ident, '{one two}', _},
                        <<>>,
                        _},
                       'joxa-cmp-lexer':'quoted-ident'(<<":'{one two}'">>, index()))).


quote_test() ->
    ?memo(?assertMatch({{quote,{ident,ok,{1,2}},{1,1}},<<>>,{1,4}},
                       'joxa-cmp-lexer':quote(<<"'ok">>, index()))),

    ?memo(?assertMatch({{quote,{list,[],{1,2}},{1,1}},<<>>,{1,4}},
                       'joxa-cmp-lexer':quote(<<"'()">>,
                                             index()))),

    ?memo(?assertMatch({{quote,{'literal-list',[],{1,2}},{1,1}},
                        <<>>,
                        {1,4}},
                       'joxa-cmp-lexer':quote(<<"'[]">>,
                                             index()))),

    ?memo(?assertMatch({{quote,{integer,123,{1,2}},{1,1}},<<>>,{1,5}},
                       'joxa-cmp-lexer':quote(<<"'123">>,
                                             index()))),

    ?memo(?assertMatch({{quote,{list,[{integer,1,_}],{1,2}},{1,1}},
                        <<>>,
                        {1,5}},
                       'joxa-cmp-lexer':quote(<<"'(1)">>, index()))),

    ?memo(?assertMatch({{quote,{tuple,[{ident,one,_},{ident,two,_}],
                                {1,2}},{1,1}},
                        <<>>,
                        {1,11}},
                       'joxa-cmp-lexer':quote(<<"'{one two}">>, index()))).

list_test() ->
    ?memo(?assertMatch({{list,[{quote,{ident,ok, _},{1,2}}],{1,1}},
                        <<>>,
                        {1,6}},
                       'joxa-cmp-lexer':list(<<"(:ok)">>, index()))),

    ?memo(?assertMatch({{list,[{integer,1,{1,2}},
                               {integer,2,{1,4}},
                               {integer,3,_}],
                         {1,1}},
                        <<>>,
                        {1,8}},
                       'joxa-cmp-lexer':list(<<"(1 2 3)">>,
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
                       'joxa-cmp-lexer':list(<<"[33 :forty (1 2) {:hello}]">>,
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
                       'joxa-cmp-lexer':list(<<"((((123) 1) 2) 3)">>,
                                            index()))),

    ?memo(?assertMatch({{list,[],{1,1}},<<>>,{1,3}},
                       'joxa-cmp-lexer':list(<<"()">>, index()))),

    ?memo(?assertMatch({{list,[{list,[{list,[],_}],{1,2}}],{1,1}},
                        <<>>,
                        {1,7}},
                       'joxa-cmp-lexer':list(<<"((()))">>, index()))).


tuple_test() ->
    ?memo(?assertMatch({{tuple,[{quote,{ident,ok, _},{1,2}}],{1,1}},
                        <<>>,
                        {1,6}},
                       'joxa-cmp-lexer':tuple(<<"{:ok}">>, index()))),

    ?memo(?assertMatch({{tuple,[{integer,1,{1,2}},
                                {integer,2,{1,4}},
                                {integer,3,_}],
                         {1,1}},
                        <<>>,
                        {1,8}},
                       'joxa-cmp-lexer':tuple(<<"{1 2 3}">>,
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
                       'joxa-cmp-lexer':tuple(<<"{33 :forty (1 2) {:hello}}">>,
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
                       'joxa-cmp-lexer':tuple(<<"{{{{123} 1} 2} 3}">>,
                                             index()))),

    ?memo(?assertMatch({{tuple,[],{1,1}},<<>>,{1,3}},
                       'joxa-cmp-lexer':tuple(<<"{}">>, index()))),

    ?memo(?assertMatch({{tuple,[{tuple,[{tuple,[], _}],{1,2}}],{1,1}},
                        <<>>,
                        {1,7}},
                       'joxa-cmp-lexer':tuple(<<"{{{}}}">>, index()))).

binary_test() ->
    ?memo(?assertMatch({{binary,[{integer,1,{1,3}},
                                 {integer,2,{1,5}},
                                 {integer,3, _}],
                         {1,1}},
                        <<>>,
                        {1,10}},
                       'joxa-cmp-lexer':binary(<<"<<1 2 3>>">>, index()))),

    ?memo(?assertMatch({{binary,[{char,97,{1,3}},
                                 {char,98,{1,6}},
                                 {char,99, _}],
                         {1,1}},
                        <<>>,
                        {1,13}},
                       'joxa-cmp-lexer':binary(<<"<<\\a \\b \\c>>">>,
                                              index()))),

    ?memo(?assertMatch({{binary,[{ident,a,{1,3}},
                                 {ident,b,{1,5}},
                                 {list,[{ident, c, _},
                                        {quote, {ident, size, _}, _},
                                        {integer, 16, _}], _}],
                         {1,1}},
                        <<>>,
                        {1,21}},
                       'joxa-cmp-lexer':binary(<<"<<a b (c :size 16)>>">>,
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
                       'joxa-cmp-lexer':binary(<<"<<(d :size 16) e (f :binary)>>">>,
                                              index()))),

    ?memo(?assertMatch({{binary,[],{1,1}},<<>>,{1,5}},
                       'joxa-cmp-lexer':binary(<<"<<>>">>, index()))),

    ?memo(?assertMatch({{binary,{string,[],{1,3}},{1,1}},
                        <<>>,
                        {1,7}},
                       'joxa-cmp-lexer':binary(<<"<<\"\">>">>,
                                              index()))),
    ?memo(?assertMatch({{binary,{string,"HelloWorld",{1,4}},{1,1}},
                        <<>>,
                        {1,19}},
                       'joxa-cmp-lexer':binary(<<"<< \"HelloWorld\" >>">>,
                                              index()))).

value_test() ->
    ?memo(?assertMatch({{ident, 'true', {1, _}}, <<>>, _},
                       'joxa-cmp-lexer':value(<<"true">>, index()))),
    ?memo(?assertMatch({{ident, 'false', {1, _}},  <<>>, _},
                       'joxa-cmp-lexer':value(<<"false">>, index()))),
    ?memo(?assertMatch({{ident, '*foo*', {1, _}}, <<>>, _},
                       'joxa-cmp-lexer':value(<<"*foo*">>, index()))),
    ?memo(?assertMatch({{ident, 'foo-bar', {1, _}}, <<>>, _},
                       'joxa-cmp-lexer':value(<<"foo-bar">>, index()))),
    ?memo(?assertMatch({{ident, 'null', {1, _}}, <<>>, _},
                       'joxa-cmp-lexer':value(<<"null">>, index()))),
    ?memo(?assertMatch({{ident, 'Hello?', {1, _}}, <<>>, _},
                       'joxa-cmp-lexer':value(<<"Hello?">>, index()))),
    ?memo(?assertMatch({{ident, 'boo88', {1, _}}, <<>>, _},
                       'joxa-cmp-lexer':value(<<"boo88">>, index()))),
    ?memo(?assertMatch({{ident, 'bock:', {1, _}}, <<>>, _},
                       'joxa-cmp-lexer':value(<<"bock:">>, index()))),
    ?memo(?assertMatch({{ident, 'bock', {1, _}}, <<"{">>, _},
                       'joxa-cmp-lexer':value(<<"bock{">>, index()))),
    ?memo(?assertMatch({{ident, 'bock', {1, _}}, <<"[">>, _},
                       'joxa-cmp-lexer':value(<<"bock[">>, index()))),
    ?memo(?assertMatch({{ident, 'bock', {1, _}}, <<"(ee">>, _},
                       'joxa-cmp-lexer':value(<<"bock(ee">>, index()))),

    ?memo(?assertMatch({{quote,{ident,true,{1,2}},{1,1}},<<>>,{1,6}},
                       'joxa-cmp-lexer':value(<<":true">>, index()))),
    ?memo(?assertMatch({{quote,{ident,false,{1,2}},{1,1}},<<>>,{1,7}},
                       'joxa-cmp-lexer':value(<<":false">>, index()))),
    ?memo(?assertMatch({{quote,{ident,'*foo*',{1,2}},{1,1}},
                        <<>>,
                        {1,7}},
                       'joxa-cmp-lexer':value(<<":*foo*">>, index()))),
    ?memo(?assertMatch({{ident,'foo-bar',{1,1}},<<>>,{1,8}},
                       'joxa-cmp-lexer':value(<<"foo-bar">>, index()))),
    ?memo(?assertMatch({{quote,{ident,'Hello?', _},{1,1}},
                        <<>>,
                        {1,8}},
                       'joxa-cmp-lexer':value(<<":Hello?">>, index()))),
    ?memo(?assertMatch({{quote,{ident,boo88,_},{1,1}},<<>>,{1,7}},
                       'joxa-cmp-lexer':value(<<":boo88">>, index()))),
    ?memo(?assertMatch({{ident,'bock:',{1,1}},<<>>,{1,6}},
                       'joxa-cmp-lexer':value(<<"bock:">>, index()))),
    ?memo(?assertMatch({{quote,{ident,bock,_},{1,1}},
                        <<"(ee">>,
                        {1,6}},
                       'joxa-cmp-lexer':value(<<":bock(ee">>, index()))),

    ?memo(?assertMatch({{call,{'--fun','fun',3},{1,1}},<<>>,{1,6}},
                       'joxa-cmp-lexer':value(<<"fun/3">>, index()))),
    ?memo(?assertMatch({{call,{'--fun',module,'fun',3},{1,1}},
                        <<>>,
                        {1,13}},
                       'joxa-cmp-lexer':value(<<"module/fun/3">>,
                                             index()))),
    ?memo(?assertMatch({{call,{'--fun',module,'fun'},_},
                        <<>>,
                        {1,11}},
                       'joxa-cmp-lexer':value(<<"module/fun">>,
                                             index()))),
    ?memo(?assertMatch({fail,
                        {expected,{'at-least-one',
                                   {'character-class',"[0-9]"}},{1,1}}},
                       'joxa-cmp-lexer':value(<<"/2">>, index()))),

    ?memo(?assertMatch({{string," \" ",{1,1}},<<>>,{1,7}},
                       'joxa-cmp-lexer':value(<<"\" \\\" \"">>, index()))),

    ?memo(?assertMatch({{string,"\\",{1,1}},<<>>,{1,5}},
                       'joxa-cmp-lexer':value(<<"\"\\\\\"">>,
                                             index()))),

    ?memo(?assertMatch({{string,"\f",{1,1}},<<>>,{1,5}},
                       'joxa-cmp-lexer':value(<<"\"\\f\"">>,
                                             index()))),

    ?memo(?assertMatch({{string,"\t",{1,1}},<<>>,{1,5}},
                       'joxa-cmp-lexer':value(<<"\"\\t\"">>,
                                             index()))),

    ?memo(?assertMatch({{string,"\n",{1,1}},<<>>,{1,5}},
                       'joxa-cmp-lexer':value(<<"\"\\n\"">>, index()))),

    ?memo(?assertMatch({{string,"\r",{1,1}},<<>>,{1,5}},
                       'joxa-cmp-lexer':value(<<"\"\\r\"">>, index()))),

    ?memo(?assertMatch({{quote,{ident,ok,{1,2}},{1,1}},<<>>,{1,4}},
                       'joxa-cmp-lexer':value(<<"'ok">>, index()))),

    ?memo(?assertMatch({{quote,{list,[],{1,2}},{1,1}},<<>>,{1,4}},
                       'joxa-cmp-lexer':value(<<"'()">>,
                                             index()))),

    ?memo(?assertMatch({{quote,{'literal-list',[],{1,2}},{1,1}},
                        <<>>,
                        {1,4}},
                       'joxa-cmp-lexer':value(<<"'[]">>,
                                             index()))),

    ?memo(?assertMatch({{quote,{integer,123,{1,2}},{1,1}},<<>>,{1,5}},
                       'joxa-cmp-lexer':value(<<"'123">>,
                                             index()))),

    ?memo(?assertMatch({{quote,{list,[{integer,1,_}],{1,2}},{1,1}},
                        <<>>,
                        {1,5}},
                       'joxa-cmp-lexer':value(<<"'(1)">>, index()))),

    ?memo(?assertMatch({{quote,{tuple,[{ident,one,_},{ident,two,_}],
                                {1,2}},{1,1}},
                        <<>>,
                        {1,11}},
                       'joxa-cmp-lexer':value(<<"'{one two}">>, index()))),

    ?memo(?assertMatch({{list,[{quote,{ident,ok, _},{1,2}}],{1,1}},
                        <<>>,
                        {1,6}},
                       'joxa-cmp-lexer':value(<<"(:ok)">>, index()))),

    ?memo(?assertMatch({{list,[{integer,1,{1,2}},
                               {integer,2,{1,4}},
                               {integer,3,_}],
                         {1,1}},
                        <<>>,
                        {1,8}},
                       'joxa-cmp-lexer':value(<<"(1 2 3)">>,
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
                       'joxa-cmp-lexer':value(<<"[33 :forty (1 2) {:hello}]">>,
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
                       'joxa-cmp-lexer':value(<<"((((123) 1) 2) 3)">>,
                                             index()))),

    ?memo(?assertMatch({{list,[],{1,1}},<<>>,{1,3}},
                       'joxa-cmp-lexer':value(<<"()">>, index()))),

    ?memo(?assertMatch({{list,[{list,[{list,[],_}],{1,2}}],{1,1}},
                        <<>>,
                        {1,7}},
                       'joxa-cmp-lexer':value(<<"((()))">>, index()))),

    ?memo(?assertMatch({{tuple,[{quote,{ident,ok, _},{1,2}}],{1,1}},
                        <<>>,
                        {1,6}},
                       'joxa-cmp-lexer':value(<<"{:ok}">>, index()))),

    ?memo(?assertMatch({{tuple,[{integer,1,{1,2}},
                                {integer,2,{1,4}},
                                {integer,3,_}],
                         {1,1}},
                        <<>>,
                        {1,8}},
                       'joxa-cmp-lexer':value(<<"{1 2 3}">>,
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
                       'joxa-cmp-lexer':value(<<"{33 :forty (1 2) {:hello}}">>,
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
                       'joxa-cmp-lexer':value(<<"{{{{123} 1} 2} 3}">>,
                                             index()))),

    ?memo(?assertMatch({{tuple,[],{1,1}},<<>>,{1,3}},
                       'joxa-cmp-lexer':value(<<"{}">>, index()))),

    ?memo(?assertMatch({{tuple,[{tuple,[{tuple,[], _}],{1,2}}],{1,1}},
                        <<>>,
                        {1,7}},
                       'joxa-cmp-lexer':value(<<"{{{}}}">>, index()))),

    ?memo(?assertMatch({{binary,[{integer,1,{1,3}},
                                 {integer,2,{1,5}},
                                 {integer,3, _}],
                         {1,1}},
                        <<>>,
                        {1,10}},
                       'joxa-cmp-lexer':value(<<"<<1 2 3>>">>, index()))),

    ?memo(?assertMatch({{binary,[{char,97,{1,3}},
                                 {char,98,{1,6}},
                                 {char,99, _}],
                         {1,1}},
                        <<>>,
                        {1,13}},
                       'joxa-cmp-lexer':value(<<"<<\\a \\b \\c>>">>,
                                             index()))),

    ?memo(?assertMatch({{binary,[{ident,a,{1,3}},
                                 {ident,b,{1,5}},
                                 {list,[{ident, c, _},
                                        {quote, {ident, size, _}, _},
                                        {integer, 16, _}], _}],
                         {1,1}},
                        <<>>,
                        {1,21}},
                       'joxa-cmp-lexer':value(<<"<<a b (c :size 16)>>">>,
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
                       'joxa-cmp-lexer':value(<<"<<(d :size 16) e (f :binary)>>">>,
                                             index()))),

    ?memo(?assertMatch({{binary,[],{1,1}},<<>>,{1,5}},
                       'joxa-cmp-lexer':value(<<"<<>>">>, index()))),

    ?memo(?assertMatch({{binary,{string,"",{1,3}},{1,1}},
                        <<>>,
                        {1,7}},
                       'joxa-cmp-lexer':value(<<"<<\"\">>">>,
                                             index()))),
    ?memo(?assertMatch({{binary,{string,"HelloWorld",{1,4}},{1,1}},
                        <<>>,
                        {1,19}},
                       'joxa-cmp-lexer':value(<<"<< \"HelloWorld\" >>">>,
                                             index()))),

    ?memo(?assertMatch({{ident, ok, _},<<>>,_},
                       'joxa-cmp-lexer':value(<<":'ok'">>, index()))),

    ?memo(?assertMatch({{ident, '()', _},<<>>,_},
                       'joxa-cmp-lexer':value(<<":'()'">>,
                                             index()))),

    ?memo(?assertMatch({{ident, '[]', _},
                        <<>>,
                        _},
                       'joxa-cmp-lexer':value(<<":'[]'">>,
                                             index()))),

    ?memo(?assertMatch({{ident, '123', _},<<>>,_},
                       'joxa-cmp-lexer':value(<<":'123'">>,
                                             index()))),

    ?memo(?assertMatch({{ident, '(1)', _},
                        <<>>,
                        _},
                       'joxa-cmp-lexer':value(<<":'(1)'">>, index()))),

    ?memo(?assertMatch({{ident, '{one two}', _},
                        <<>>,
                        _},
                       'joxa-cmp-lexer':value(<<":'{one two}'">>, index()))).

%%%===================================================================
%%% Support Functions
%%%===================================================================
index() ->
    {1, 1}.

index(N) ->
    {1+N, 1+N}.
