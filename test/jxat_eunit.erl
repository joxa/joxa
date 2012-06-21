%%%-------------------------------------------------------------------
%%% @author Eric B Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2012, Eric B Merritt
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(jxat_eunit).

%% API
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================
under_eunit_test() ->
    Source = <<"(ns jxat-eunit-under-eunit-test
                     (require joxa-eunit))

                (defn+ under-eunit-pass ()
                    (joxa-eunit/under-eunit?))">>,

    Ctx = 'joxa-compiler':forms(Source, []),
    ?assertMatch(true,is_binary('joxa-compiler':'get-context'(result, Ctx))),
    ?assertMatch(true, 'jxat-eunit-under-eunit-test':'under-eunit-pass'()).

test_test() ->
    Source = <<"(ns jxat-eunit-test-test
                     (require joxa-eunit))

                (defn+ test-test ()
                    (joxa-eunit/-test :foo))">>,

    Ctx = 'joxa-compiler':forms(Source, []),
    ?assertMatch(true,is_binary('joxa-compiler':'get-context'(result, Ctx))),
    ?assertMatch({5,_}, 'jxat-eunit-test-test':'test-test'()),
    {_, Fun} = 'jxat-eunit-test-test':'test-test'(),
    ?assertMatch(foo, Fun()).

assert_test() ->
    Source = <<"(ns jxat-eunit-assert-test
                     (require joxa-eunit))

                (defn+ assert-test ()
                    (joxa-eunit/-assert (erlang/== :foo :bar)))">>,

    Ctx = 'joxa-compiler':forms(Source, []),
    ?assertMatch(true,is_binary('joxa-compiler':'get-context'(result, Ctx))),
    ?assertMatch({5,_}, 'jxat-eunit-assert-test':'assert-test'()),
    {_, Fun} = 'jxat-eunit-assert-test':'assert-test'(),
    ?assertError({assertion_failed,[{namespace,'jxat-eunit-assert-test'},
                                    {line,5},
                                    {expression,[{'--fun',erlang,'=='},
                                                 [quote,foo],
                                                 [quote,bar]]},
                                    {expected,[quote,true]},
                                    {value,false}]},
                 Fun()).


assert_not_test() ->
    Source = <<"(ns jxat-eunit-assert-not-test
                     (require joxa-eunit))

                (defn+ assert-not-test ()
                    (joxa-eunit/-assert-not (erlang/== :foo :foo)))">>,

    Ctx = 'joxa-compiler':forms(Source, []),
    ?assertMatch(true,is_binary('joxa-compiler':'get-context'(result, Ctx))),
    ?assertMatch({5,_}, 'jxat-eunit-assert-not-test':'assert-not-test'()),
    {_, Fun} = 'jxat-eunit-assert-not-test':'assert-not-test'(),
    ?assertError({assertion_failed,[{namespace,'jxat-eunit-assert-not-test'},
                                    {line,5},
                                    {expression,[{'--fun',erlang,'not'},
                                                 [{'--fun',erlang,'=='},
                                                  [quote,foo],
                                                  [quote,foo]]]},
                                    {expected,[quote,true]},
                                    {value,false}]},
                 Fun()).


cmd_test() ->
    Source = <<"(ns jxat-eunit-cmd-test
                     (require joxa-eunit))

                (defn+ cmd-test ()
                    (joxa-eunit/-cmd- \"erl -s init stop\"))">>,

    Ctx = 'joxa-compiler':forms(Source, []),
    ?assertMatch(true,is_binary('joxa-compiler':'get-context'(result, Ctx))),
    ?assertMatch({0, [$E, $s, $h, $e, $l, $l |_]}, 'jxat-eunit-cmd-test':'cmd-test'()).

cmd_status_test() ->
    Source = <<"(ns jxat-eunit-cmd-status-test
                     (require joxa-eunit))

                (defn+ cmd-status-test ()
                    (joxa-eunit/cmd-status  0 \"erl -s init stop\"))

                (defn+ cmd-status-test2 ()
                    (joxa-eunit/cmd-status  1 \"erl -s init stop\"))">>,

    Ctx = 'joxa-compiler':forms(Source, []),
    ?assertMatch(true,is_binary('joxa-compiler':'get-context'(result, Ctx))),
    ?assertMatch([$E, $s, $h, $e, $l, $l |_],
                 'jxat-eunit-cmd-status-test':'cmd-status-test'()),

    ?assertError({command_failed,[{namespace,'jxat-eunit-cmd-status-test'},
                                  {line,8},
                                  {command,"erl -s init stop"},
                                  {expected_status,1},
                                  {status,0}]},
                 'jxat-eunit-cmd-status-test':'cmd-status-test2'()).


assert_cmd_status_test() ->
    Source = <<"(ns jxat-eunit-assert-cmd-status-test
                     (require joxa-eunit))

                (defn+ assert-cmd-status-test ()
                    (joxa-eunit/assert-cmd-status  0 \"erl -s init stop\"))

                (defn+ assert-cmd-status-test2 ()
                    (joxa-eunit/assert-cmd-status  1 \"erl -s init stop\"))">>,

    Ctx = 'joxa-compiler':forms(Source, []),
    ?assertMatch(true,is_binary('joxa-compiler':'get-context'(result, Ctx))),
    ?assertMatch([$E, $s, $h, $e, $l, $l |_],
                 'jxat-eunit-assert-cmd-status-test':'assert-cmd-status-test'()),
    ?assertError({assertCmd_failed,[{namespace,'jxat-eunit-assert-cmd-status-test'},
                                    {line,8},
                                    {command,"erl -s init stop"},
                                    {expected_status,1},
                                    {status,0}]},
                  'jxat-eunit-assert-cmd-status-test':'assert-cmd-status-test2'()).

assert_cmd_output_test() ->
    Source = <<"(ns jxat-eunit-assert-cmd-output-test
                     (require joxa-eunit))

                (defn+ assert-cmd-output-test ()
                    (joxa-eunit/assert-cmd-output  (\\E . (\\s . (\\h . (\\e . (\\l . (\\l . _)))))) \"erl -s init stop\"))

                (defn+ assert-cmd-output-test2 ()
                    (joxa-eunit/assert-cmd-output  \"Foo!\" \"erl -s init stop\"))">>,

    Ctx = 'joxa-compiler':forms(Source, []),
    ?assertMatch(true,is_binary('joxa-compiler':'get-context'(result, Ctx))),
    ?assertMatch(ok,
                 'jxat-eunit-assert-cmd-output-test':'assert-cmd-output-test'()),
    ?assertError({assertCmdOutput_failed,
                  [{namespace,'jxat-eunit-assert-cmd-output-test'},
                   {line,8},
                   {command,"erl -s init stop"},
                   {expected_output,"Foo!"},
                   {output, [$E, $s, $h | _]}]},
                 'jxat-eunit-assert-cmd-output-test':'assert-cmd-output-test2'()).

debug_msg_test() ->
    Source = <<"(ns jxat-eunit-debug-msg-test
                     (require joxa-eunit))

                (defn+ debug-msg-test ()
                    (joxa-eunit/debug-msg \"woo-hoo\"))

                (defn+ debug-here()
                    (joxa-eunit/debug-here))

                (defn+ debug-fmt()
                    (joxa-eunit/debug-fmt \"~p~n\" [:foo]))

                (defn+ debug-val()
                    (joxa-eunit/debug-val \"super\"))

                (defn+ debug-time()
                    (joxa-eunit/debug-time \"foo\" (erlang/+ 1 2)))
">>,

    Ctx = 'joxa-compiler':forms(Source, []),
    ?assertMatch(true,is_binary('joxa-compiler':'get-context'(result, Ctx))),
    ?assertMatch(ok,
                 'jxat-eunit-debug-msg-test':'debug-msg-test'()),
    ?assertMatch(ok,
                 'jxat-eunit-debug-msg-test':'debug-here'()),

    ?assertMatch(ok,
                 'jxat-eunit-debug-msg-test':'debug-fmt'()),
    ?assertMatch("super",
                 'jxat-eunit-debug-msg-test':'debug-val'()),

    ?assertMatch(3,
                 'jxat-eunit-debug-msg-test':'debug-time'()).





