-module(jxat_case).

-export([given/3, 'when'/3, then/3]).
-include_lib("eunit/include/eunit.hrl").

given([a,module,that,has,a,'case',statement], _State, _) ->
    Source = <<"(ns jxat-case-test
                    (use (erlang :only (==/2 phash2/1 and/2))))

                (defn internal-test (arg1 arg2)
                      (case {arg1 arg2}
                         ({:foo :bar}
                           (phash2 :bar))
                         (_z
                            :ok)))
                (defn internal-test2 (arg1 arg2 arg3)
                       (case [arg1 arg2 arg3]
                          ([one two three]
                              (when (and (and (erlang/is_atom one)
                                        (erlang/is_integer two))
                                   (erlang/is_list three)))
                            (phash2 one))
                          ([{a a 33} a 4]
                              (phash2 a))
                          ([:foo :bar _]
                           (phash2 :bar))))

                (defn+ do-test (arg1 arg2 arg3)
                      (let (z (internal-test arg1 arg2)
                            x (internal-test2 arg1 arg2 arg3))
                           {z x}))">>,

    {ok, Source}.

'when'([joxa,is,called,on,this,module], Source, _) ->
    Result = joxa.compiler:forms(Source, []),
    {ok, Result}.

then([a,beam,binary,is,produced], Ctx, _) ->
      ?assertMatch(true, is_binary(joxa.compiler:'get-context'(result, Ctx))),
    {ok, Ctx};
then([the,described,function,can,be,called,'and',works,correctly], State, _) ->
    ?assertMatch([{'--joxa-info',1},
                  {'--joxa-info',2},
                  {'do-test',3},
                  {module_info,0},
                  {module_info,1}],
                 lists:sort('jxat-case-test':module_info(exports))),
    ?assertMatch({26754, 26754}, 'jxat-case-test':'do-test'(foo, bar, 4)),
    ?assertMatch({ok, 73439361}, 'jxat-case-test':'do-test'({22, 22, 33}, 22, 4)),
    ?assertMatch({ok, 4557629}, 'jxat-case-test':'do-test'(an_atom, 22, [1, 2, 3])),
    {ok, State}.


