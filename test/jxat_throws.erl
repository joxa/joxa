-module(jxat_throws).

-export([given/3, 'when'/3, then/3]).
-include_lib("eunit/include/eunit.hrl").

given([a,module,that,catches,an,exception], _State, _) ->
    Source = <<"(module jxat-throws-test
                    (use (erlang :only (throw/1))))

                (defn internal-test ()
                    (throw {:this :is :a :test}))

                (defn+ do-test1 ()
                      (try (do (internal-test) :nope)
                       (catch
                          (:throw {:this :is :a :test}
                           :got-it))))
                (defn+ do-test2 ()
                      (try (do (internal-test) :nope)
                       (catch
                         (:throw {:this :is :not}
                                  :got-it))))">>,

    {ok, Source}.


'when'([joxa,is,called,on,this,module], Source, _) ->
    Result = jxa_compile:comp(Source),
    {ok, Result}.

then([a,beam,binary,is,produced], State = {_, Binary},  _) ->
      ?assertMatch(true, is_binary(Binary)),
    {ok, State};
then([the,described,function,can,be,called,'and',works,correctly], State, _) ->
    ?assertMatch([{'do-test1',0},
                  {'do-test2',0},
                  {module_info,0},
                  {module_info,1}],
                 lists:sort('jxat-throws-test':module_info(exports))),
    ?assertMatch('got-it', 'jxat-throws-test':'do-test1'()),
    ?assertThrow({this,is,a,test},
                 'jxat-throws-test':'do-test2'()),
    {ok, State}.


