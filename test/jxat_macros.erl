-module(jxat_macros).

-export([given/3, 'when'/3, then/3]).
-include_lib("eunit/include/eunit.hrl").

given([a,module,that,contains,macros], _State, _) ->
    Source = <<"(ns jxat-macro-test
                    (require erlang))

                (defmacro+ test1 (foo a)
                   `(defn+ ~foo ()
                        (erlang/+ 22 ~a)))

                (defmacro+ test2 ()
                   `(do (test1 gen-test1 22)
                        (defn+ gen-test2 ()
                            :ok)))

               (test2)

                 (defmacro+ when (test &rest body)
                       `(case ~test
                           (:true
                               ~@body)
                           (:false
                               :ok)
                           (_
                                (erlang/throw :invalid-when))))

                (defn+ test3 (a)
                    (when a
                        :got-it))


                (defmacro pattern-mac (super)
                      `{~super})

                (defn+ test4 (some)
                     (case {:foo}
                       ((pattern-mac some)
                           some)
                       (_
                          :other)))
">>,
    {ok, Source};
given([a,module,that,contains,a,macro,that,errors], _State, _) ->
    Source = <<"(ns jxat-error-macro-test
                    (require erlang))

                (defmacro+ test1 (foo a)
                     (erlang/error :this-is-an-error))

                 (defn+ test2 (a)
                    (test1 1 a))
">>,
    {ok, Source}.

'when'([joxa,is,called,on,this,module], Source, _) ->
    Result = 'joxa-compiler':forms(Source, []),
    {ok, Result}.
then([a,beam,binary,is,produced], Ctx, _) ->
    ?assertMatch(true, is_binary('joxa-compiler':'get-context'(result, Ctx))),
    {ok, Ctx};
then([an,error,is,produced], Ctx, _) ->
    ?assertMatch(true, 'joxa-compiler':'has-errors?'(Ctx)),
    {ok, Ctx};
then([that,error,is,in,the,error,list], Ctx, _) ->
    ErrorList = 'joxa-compiler':'get-context'(errors, Ctx),
    ?assert(lists:any(fun(Err) ->
                              case Err of
                                  {{'macro-failure',{'jxat-error-macro-test',test1,2},
                                   {error,'this-is-an-error', _}}, _} ->
                                      true;
                                  _ ->
                                      false
                              end
                      end, ErrorList)),
    {ok, Ctx};
then([the,described,function,can,be,called,'and',works,correctly], State, _) ->
    ?assertMatch([{'--joxa-info',1},
                  {'--joxa-info',2},
                  {'gen-test1',0},
                  {'gen-test2',0},
                  {module_info,0},
                  {module_info,1},
                  {'test1',2},
                  {'test2',0},
                  {'test3',1},
                  {'test4',1},
                  {'when',2}],
                 lists:sort('jxat-macro-test':module_info(exports))),
    ?assertMatch([{'pattern-mac',1}, {test1,2}, {test2,0}, {'when',2}],
    lists:sort('jxat-macro-test':'--joxa-info'(macro))),
    ?assertMatch(true, 'jxat-macro-test':'--joxa-info'(macro, {test1, 2})),
    ?assertMatch(true, 'jxat-macro-test':'--joxa-info'(macro, {test2, 0})),
    ?assertMatch(true, 'jxat-macro-test':'--joxa-info'(macro, {'when', 2})),
    ?assertMatch(false, 'jxat-macro-test':'--joxa-info'(macro, {test3, 0})),
    ?assertMatch(['defn+',hello,[],[{'--fun',erlang,'+'},22,23]],
                 'jxat-macro-test':test1(hello, 23)),
    ?assertMatch([do,
                  [test1,'gen-test1',22],
                  ['defn+','gen-test2',[],[quote,ok]]],
                 'jxat-macro-test':test2()),
    ?assertMatch(['case',hello,
                  [[quote,true],one,two,three],
                  [[quote,false],[quote,ok]],
                  ['_',[{'--fun',erlang,throw},[quote,'invalid-when']]]],
                 'jxat-macro-test':'when'(hello, [one, two, three])),

    ?assertMatch('got-it', 'jxat-macro-test':'test3'(true)),
    ?assertMatch(ok, 'jxat-macro-test':'test3'(false)),
    ?assertThrow('invalid-when', 'jxat-macro-test':'test3'(something)),
    ?assertMatch(44, 'jxat-macro-test':'gen-test1'()),
    ?assertMatch(ok, 'jxat-macro-test':'gen-test2'()),
    ?assertMatch(foo, 'jxat-macro-test':'test4'(foo)),
    ?assertMatch(other, 'jxat-macro-test':'test4'(bar)),
    {ok, State}.
