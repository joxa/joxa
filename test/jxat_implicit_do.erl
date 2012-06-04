-module(jxat_implicit_do).

-export([given/3, 'when'/3, then/3]).
-include_lib("eunit/include/eunit.hrl").

given([a,module,that,has,an,anonymous,function], _State, _) ->
        Source = <<"(ns jxat-implicit-do-test
                       (require io))

                (defn t1 ()
                      (let (a 1)
                         (io/format \"~p\" [a])
                         :booha))

                (defn+ do-test ()
                    (case (t1)
                      (:booha
                          (io/format \"did it\")
                           :return-it)))
                (defn+ do-test2 ()
                    (t1)
                    (do-test))">>,
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
                    {'do-test',0},
                    {'do-test2',0},
                    {module_info,0},
                    {module_info,1}],
                 lists:sort('jxat-implicit-do-test':module_info(exports))),
    {ok, State}.

