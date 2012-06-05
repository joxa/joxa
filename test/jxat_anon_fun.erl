-module(jxat_anon_fun).

-export([given/3, 'when'/3, then/3]).
-include_lib("eunit/include/eunit.hrl").

given([a,module,that,has,an,anonymous,function], _State, _) ->
    Source = <<"(ns jxat-anon-fun
                    (use (erlang :only (==/2 phash2/1))))

                (defn internal-test ()
                      (fn (arg1 arg2)
                        {:hello arg1 arg2}))

                (defn+ do-test ()
                      (let (z (internal-test)
                            c '(one two three)
                            x (fn (arg1 arg2)
                                {:hello arg1 arg2})
                            a erlang/phash2/1
                            y (x 1 2))
                           (do
                                (apply internal-test/0)
                                (apply x 1 2)
                                (apply a 22)
                                (a 22))))">>,

    {ok, Source}.

'when'([joxa,is,called,on,this,module], Source, _) ->
    Result = 'joxa-compiler':forms(Source, []),
    {ok, Result}.
then([a,beam,binary,is,produced], Ctx, _) ->
    ?assertMatch(true, is_binary('joxa-compiler':'get-context'(result, Ctx))),
    {ok, Ctx};
then([the,described,function,can,be,called,'and',works,correctly],
     State, _) ->
    ?assertMatch([{'--joxa-info',1},
                  {'--joxa-info',2},
                  {'do-test',0},
                  {module_info,0},
                  {module_info,1}],
                 lists:sort('jxat-anon-fun':module_info(exports))),
    {ok, State}.

