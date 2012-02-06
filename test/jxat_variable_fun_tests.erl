-module(jxat_variable_fun_tests).

-include_lib("eunit/include/eunit.hrl").

fun_var_test() ->
        Source = <<"
(module jxat-fun-var-test )

  (defn get-it0 ()
     (let (f (fn (a b) {a b}))
        f))

  (defn get-it1 ()
     (let (f (fn (a b) {a b}))
        f/2))

  (defn+ test-case0 ()
      ((get-it0) 1 2))

  (defn+ test-case1 ()
      ((get-it1) 1 2))">>,

    Ctx = joxa.compiler:forms(Source, []),
    ?assertMatch(true, is_binary(joxa.compiler:'get-context'(result, Ctx))),
    ?assertMatch({1, 2},
                 'jxat-fun-var-test':'test-case0'()),
    ?assertMatch({1, 2},
                 'jxat-fun-var-test':'test-case1'()).


