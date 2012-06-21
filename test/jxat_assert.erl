%%%-------------------------------------------------------------------
%%% @author Eric B Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2012, Eric B Merritt
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(jxat_assert).

%% API
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================
matches_test() ->
    Source = <<"(ns jxat-matches-test
                     (require joxa-assert))

                (defn+ matches ()
                    (joxa-assert/matches :foo :foo))

                (defn+ does-not-match()
                    (joxa-assert/matches :foo :baz))">>,
    Ctx = 'joxa-compiler':forms(Source, []),
    ?assertMatch(true,is_binary('joxa-compiler':'get-context'(result, Ctx))),
    ?assertMatch(true, 'jxat-matches-test':matches()),
    ?assertMatch(false, 'jxat-matches-test':'does-not-match'()).

assert_test() ->
    Source = <<"(ns jxat-assert-test
                     (require joxa-assert))

                (defn+ assert-pass()
                    (joxa-assert/assert :true))

                (defn+ assert-fail()
                    (joxa-assert/assert :false))">>,
    Ctx = 'joxa-compiler':forms(Source, []),
    ?assertMatch(true,is_binary('joxa-compiler':'get-context'(result, Ctx))),
    ?assertMatch(ok, 'jxat-assert-test':'assert-pass'()),
    ?assertError({assertion_failed,[{namespace,'jxat-assert-test'},
                                    {line,8},
                                    {expression,[quote,false]},
                                    {expected,[quote,true]},
                                    {value,false}]},
                 'jxat-assert-test':'assert-fail'()).

assert_match_test() ->
    Source = <<"(ns jxat-assert-match-test
                     (require joxa-assert))

                (defn+ assert-match-pass()
                    (joxa-assert/assert-match :true ((fn () :true))))

                (defn+ assert-match-fail()
                    (joxa-assert/assert-match :false ((fn () :true))))">>,
    Ctx = 'joxa-compiler':forms(Source, []),
    ?assertMatch(true,is_binary('joxa-compiler':'get-context'(result, Ctx))),
    ?assertMatch(ok, 'jxat-assert-match-test':'assert-match-pass'()),

    ?assertError({assertMatch_failed,[{namespace,'jxat-assert-match-test'},
                                      {line,8},
                                      {expression,[[fn,[],[quote,true]]]},
                                      {pattern,[quote,false]},
                                      {value,true}]},
                 'jxat-assert-match-test':'assert-match-fail'()).


assert_not_match_test() ->
    Source = <<"(ns jxat-assert-not-match-test
                     (require joxa-assert))

                (defn+ assert-not-match-pass()
                    (joxa-assert/assert-not-match :false ((fn () :true))))

                (defn+ assert-not-match-fail()
                    (joxa-assert/assert-not-match :true ((fn () :true))))">>,
    Ctx = 'joxa-compiler':forms(Source, []),
    ?assertMatch(true,is_binary('joxa-compiler':'get-context'(result, Ctx))),
    ?assertMatch(ok, 'jxat-assert-not-match-test':'assert-not-match-pass'()),
    ?assertError({assertNotMatch_failed,[{namespace,'jxat-assert-not-match-test'},
                                         {line,8},
                                         {expression,[[fn,[],[quote,true]]]},
                                         {pattern,[quote,true]},
                                         {value,true}]},
                 'jxat-assert-not-match-test':'assert-not-match-fail'()).


assert_equal_test() ->
    Source = <<"(ns jxat-assert-equal-test
                     (require joxa-assert))

                (defn+ assert-equal-pass()
                    (joxa-assert/assert-equal :true ((fn () :true))))

                (defn+ assert-equal-fail()
                    (joxa-assert/assert-equal :false ((fn () :true))))">>,
    Ctx = 'joxa-compiler':forms(Source, []),
    ?assertMatch(true,is_binary('joxa-compiler':'get-context'(result, Ctx))),
    ?assertMatch(ok, 'jxat-assert-equal-test':'assert-equal-pass'()),
    ?assertError({assertEqual_failed,[{namespace,'jxat-assert-equal-test'},
                                      {line,8},
                                      {expression,[[fn,[],[quote,true]]]},
                                      {expected,false},
                                      {value,true}]},
                 'jxat-assert-equal-test':'assert-equal-fail'()).


assert_not_equal_test() ->
    Source = <<"(ns jxat-assert-not-equal-test
                     (require joxa-assert))

                (defn+ assert-not-equal-pass()
                    (joxa-assert/assert-not-equal :false ((fn () :true))))

                (defn+ assert-not-equal-fail()
                    (joxa-assert/assert-not-equal :true ((fn () :true))))">>,
    Ctx = 'joxa-compiler':forms(Source, []),
    ?assertMatch(true,is_binary('joxa-compiler':'get-context'(result, Ctx))),
    ?assertMatch(ok, 'jxat-assert-not-equal-test':'assert-not-equal-pass'()),
    ?assertError({assertNotEqual_failed,[{namespace,'jxat-assert-not-equal-test'},
                                         {line,8},
                                         {expression,[[fn,[],[quote,true]]]},
                                         {value,true}]},
                 'jxat-assert-not-equal-test':'assert-not-equal-fail'()).

assert_exception_test() ->
    Source = <<"(ns jxat-assert-exception-test
                     (require joxa-assert))

                (defn+ assert-exception-pass()
                    (joxa-assert/assert-exception :throw :foo-bar
                           (erlang/throw :foo-bar)))

                (defn+ assert-exception-fail()
                    (joxa-assert/assert-exception :error :foo-bar
                           (erlang/throw :not-foo-bar)))">>,
    Ctx = 'joxa-compiler':forms(Source, []),
    ?assertMatch(true,is_binary('joxa-compiler':'get-context'(result, Ctx))),
    ?assertMatch(ok, 'jxat-assert-exception-test':'assert-exception-pass'()),
    ?assertError({assertException_failed,
                  [{namespace,'jxat-assert-exception-test'},
                   {line,_},
                   {expression,[{'--fun',erlang,throw},[quote,'not-foo-bar']]},
                   {pattern,{[quote,error],[quote,'foo-bar']}},
                   {unexpected_exception,
                    {throw,'not-foo-bar',
                     [{'jxat-assert-exception-test','assert-exception-fail',0, _}
                      | _]}}]},
                 'jxat-assert-exception-test':'assert-exception-fail'()).

assert_error_test() ->
    Source = <<"(ns jxat-assert-error-test
                     (require joxa-assert))

                (defn+ assert-error-pass()
                    (joxa-assert/assert-error :foo-bar
                           (erlang/error :foo-bar)))

                (defn+ assert-error-fail()
                    (joxa-assert/assert-error :foo-bar
                           (erlang/throw :foo-bar)))">>,
    Ctx = 'joxa-compiler':forms(Source, []),
    ?assertMatch(true,is_binary('joxa-compiler':'get-context'(result, Ctx))),
    ?assertMatch(ok, 'jxat-assert-error-test':'assert-error-pass'()),
    ?assertError({assertException_failed,
                  [{namespace,'jxat-assert-error-test'},
                   {line,_},
                   {expression,[{'--fun',erlang,throw},[quote,'foo-bar']]},
                   {pattern,{[quote,error],[quote,'foo-bar']}},
                   {unexpected_exception,
                    {throw,'foo-bar',
                     [{'jxat-assert-error-test','assert-error-fail',0, _} | _]}}]},
                 'jxat-assert-error-test':'assert-error-fail'()).


assert_exit_test() ->
    Source = <<"(ns jxat-assert-exit-test
                     (require joxa-assert))

                (defn+ assert-exit-pass()
                    (joxa-assert/assert-exit :foo-bar
                           (erlang/exit :foo-bar)))

                (defn+ assert-exit-fail()
                    (joxa-assert/assert-exit :foo-bar
                           (erlang/throw :foo-bar)))">>,
    Ctx = 'joxa-compiler':forms(Source, []),
    ?assertMatch(true,is_binary('joxa-compiler':'get-context'(result, Ctx))),
    ?assertMatch(ok, 'jxat-assert-exit-test':'assert-exit-pass'()),
    ?assertError({assertException_failed,
                  [{namespace,'jxat-assert-exit-test'},
                   {line,_},
                   {expression,[{'--fun',erlang,throw},[quote,'foo-bar']]},
                   {pattern,{[quote,exit],[quote,'foo-bar']}},
                   {unexpected_exception,
                    {throw,'foo-bar',
                     [{'jxat-assert-exit-test','assert-exit-fail',0, _} | _]}}]},
                 'jxat-assert-exit-test':'assert-exit-fail'()).

assert_throw_test() ->
    Source = <<"(ns jxat-assert-throw-test
                     (require joxa-assert))

                (defn+ assert-throw-pass()
                    (joxa-assert/assert-throw :foo-bar
                           (erlang/throw :foo-bar)))

                (defn+ assert-throw-fail()
                    (joxa-assert/assert-throw :foo-bar
                           (erlang/exit :foo-bar)))">>,
    Ctx = 'joxa-compiler':forms(Source, []),
    ?assertMatch(true,is_binary('joxa-compiler':'get-context'(result, Ctx))),
    ?assertMatch(ok, 'jxat-assert-exit-test':'assert-exit-pass'()),
    try 'jxat-assert-exit-test':'assert-exit-fail'() catch E:F ->
                                                             io:format("~p:~p~n", [E, F])
                                                     end,
    ?assertError({assertException_failed,
                  [{namespace,'jxat-assert-exit-test'},
                   {line,_},
                   {expression,[{'--fun',erlang,throw},[quote,'foo-bar']]},
                   {pattern,{[quote,exit],[quote,'foo-bar']}},
                   {unexpected_exception,
                    {throw,'foo-bar',
                     [{'jxat-assert-exit-test','assert-exit-fail',0, _} | _]}}]},
                 'jxat-assert-exit-test':'assert-exit-fail'()).

assert_not_exception_test() ->
    Source = <<"(ns jxat-assert-not-exception-test
                     (require joxa-assert))

                (defn+ assert-not-exception-pass()
                    (joxa-assert/assert-not-exception (:throw) (:foo-bar)
                           (erlang/throw :food-bar)))

                (defn+ assert-not-exception-fail()
                    (joxa-assert/assert-not-exception (:throw) (:foo-bar)
                           (erlang/throw :foo-bar)))">>,
    Ctx = 'joxa-compiler':forms(Source, []),
    ?assertMatch(true,is_binary('joxa-compiler':'get-context'(result, Ctx))),
    ?assertMatch(ok, 'jxat-assert-not-exception-test':'assert-not-exception-pass'()),
    ?assertError({assertNotException_failed,
                  [{namespace,'jxat-assert-not-exception-test'},
                   {line,_},
                   {expression,[{'--fun',erlang,throw},[quote,'foo-bar']]},
                   {pattern,{[[quote,throw]],[[quote,'foo-bar']]}},
                   {unexpected_exception,
                    {throw,'foo-bar',
                     [{'jxat-assert-not-exception-test',
                       'assert-not-exception-fail',0, _}|_]}}]},
                 'jxat-assert-not-exception-test':'assert-not-exception-fail'()).
%%%===================================================================
%%% Support Functions
%%%===================================================================
