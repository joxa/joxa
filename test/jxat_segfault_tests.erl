-module(jxat_segfault_tests).

%% This is a set of tests that, in other iterations of the compiler
%% have caused erlang to segfault. They are in the enuit tests as
%% guard to protect against these kinds of problems creeping back
%% in. At first glance they seem like usless tests. However, that is
%% not the case at all.
-include_lib("eunit/include/eunit.hrl").

bad_arity_test() ->
    Source = <<" (ns jxat-invalid-arity-test)

                  (defn rest-used-function-ctx? (name uses)
                      :not-a-reference)

                 (defn used-function-ctx? (name possible-arity ctx)
                      (let* (uses [])
                          (case (rest-used-function-ctx? name possible-arity uses)
                                (:not-a-reference
                                    :not-a-reference)
                                (result
                                   result))))">>,
    RawCtx = 'joxa-compiler':forms(Source, []),
    ?assertMatch(true, 'joxa-compiler':'has-errors?'(RawCtx)),
    ?assertMatch([{{'invalid-reference',{'rest-used-function-ctx?',3}},
                   {[],_}}],
                 'joxa-compiler':'get-context'(errors, RawCtx)).


bad_call_test() ->
    Source = <<" (ns jxat-invalid-arity-test1)

                (defn+ invalid-code-test ()
                      (let* (x 1)
                           -x))">>,
    RawCtx = 'joxa-compiler':forms(Source, []),
    ?assertMatch(true, 'joxa-compiler':'has-errors?'(RawCtx)),
    ?assertMatch([{{'invalid-reference','not-a-reference','-x'},
                              {[],_}}]
                 , 'joxa-compiler':'get-context'(errors, RawCtx)).



segfault_test() ->
        Source = <<"

(ns jxat-invalid-arity-test2 (require erlang))

  (defn is-rest-var? (c)
        :ok)

  (defn resolve (b)
       b)

  (defn+ test-case (a)
          (case a
          ({ctx1 arg-list}
             (case (resolve arg-list)
               ({:reference var}
                 :ok)
               ({:apply name arity}
                 :ok)
               ({:apply-rest name arity}
                 :ok)
               ({:remote module function possible-arity}
                 :ok)
               ({:remote-rest module function arity}
                 :ok)
               ({:error error}
                (erlang/throw {error {3 3}}))
              (:not-a-reference
                (case (resolve arg-list)
                  ({:reference var}
                    {ctx1 :ok})
                   (_
                      (erlang/throw {:invalid-reference :ok
                                                   {3 3}}))))))))">>,

    Ctx = 'joxa-compiler':forms(Source, []),
    ?assertMatch(true, is_binary('joxa-compiler':'get-context'(result, Ctx))),
    ?assertThrow({'invalid-reference', ok, _},
                 'jxat-invalid-arity-test2':'test-case'({ok, 'not-a-reference'})).

bad_let_test() ->
    Source = <<" (ns jxat-bad-let-test)

                  (defn+ rest-used-function-ctx? ()
                      (let* (x 1)
                       x)) ">>,
    RawCtx = 'joxa-compiler':forms(Source, []),
    ?assertMatch(false, 'joxa-compiler':'has-errors?'(RawCtx)),
    ?assertMatch(1, 'jxat-bad-let-test':'rest-used-function-ctx?'()).
