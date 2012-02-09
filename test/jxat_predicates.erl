-module(jxat_predicates).

-export([given/3, 'when'/3, then/3]).
-include_lib("eunit/include/eunit.hrl").

given([a,module,that,has,a,predicates], _State, _) ->
      Source = <<"(module jxat-predicate-test
                    (use (joxa.core :only (if/3 unless/2 when/2))))

                (defn+ do-if-test (val)
                  (if val
                    :was-true
                    :was-false))

                (defn+ do-when-test (val)
                  (when val
                    :hit))

                (defn+ do-unless-test (val)
                  (unless val
                    :hit))">>,

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
                  {'do-if-test',1},
                  {'do-unless-test',1},
                  {'do-when-test',1},
                  {module_info,0},
                  {module_info,1}],
                 lists:sort('jxat-predicate-test':module_info(exports))),
    ?assertMatch('was-true', 'jxat-predicate-test':'do-if-test'(true)),
    ?assertMatch('was-false', 'jxat-predicate-test':'do-if-test'(false)),
    ?assertMatch(hit, 'jxat-predicate-test':'do-when-test'(true)),
    ?assertMatch(ok, 'jxat-predicate-test':'do-when-test'(false)),
    ?assertMatch(ok, 'jxat-predicate-test':'do-unless-test'(true)),
    ?assertMatch(hit, 'jxat-predicate-test':'do-unless-test'(false)),
    {ok, State}.

