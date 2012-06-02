-module(jxat_try).

-export([given/3, 'when'/3, then/3]).
-include_lib("eunit/include/eunit.hrl").

given([a,module,that,has,a,function,that,contains,'\'try\''], _State, _) ->
     Source = <<"(ns jxat-try-test
                    (require erlang io
                             (joxa.core :as core)))


                (defn+ try-test1()
                    (core/try
                      :ok
                      (erlang/throw :something)
                      (catch
                        ({:throw :something}
                              :got-it))))

                (defn+ try-test2()
                    (core/try
                      :ok
                      (erlang/exit :something)
                      (catch
                        ({:exit :something}
                              :got-it))))

                (defn+ try-test3()
                    (core/try
                      :ok1
                      :ok2
                      :ok3
                      (catch
                        ({:exit :something}
                              :got-it))))
">>,

    {ok, Source}.

'when'([joxa,is,called,on,this,module], Source, _) ->
    Result = joxa.compiler:forms(Source, []),
    {ok, Result}.

then([a,beam,binary,is,produced], Ctx, _) ->
    ?assertMatch(true, is_binary(joxa.compiler:'get-context'(result, Ctx))),
    ?assertMatch(false, joxa.compiler:'has-errors?'(Ctx)),
    {ok, Ctx};
then([the,described,function,can,be,called,'and',works,correctly], State, _) ->
    ?assertMatch('got-it', 'jxat-try-test':'try-test1'()),
    ?assertMatch('got-it', 'jxat-try-test':'try-test2'()),
    ?assertMatch(ok3, 'jxat-try-test':'try-test3'()),
    {ok, State}.

