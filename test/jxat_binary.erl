-module(jxat_binary).

-export([given/3, 'when'/3, then/3]).
-include_lib("eunit/include/eunit.hrl").

given([a,module,that,has,a,binary,representatino], _State, _) ->
      Source = <<"(ns jxat-binary-test
                    (use (erlang :only (==/2 and/2))))

                (defn internal-test ()
                    <<\\a \\b \\c>>)

                (defn internal-test2 ()
                       (let (a 1
                             b 17
                             c 42)
                            <<a b (c :size 16)>>))

                (defn+ do-test1 ()
                      (case (internal-test)
                        (<<a b c>>
                           {a b c})))
                (defn+ do-test2 ()
                      (case (internal-test2)
                        (<<a b (c :size 16)>>
                           {a b c})))
                (defn+ do-test3 ()
                      (case (internal-test2)
                        (<<(d :size 16) e (f :binary)>>
                           {d e f})))

               (defn+ do-test4 ()
                   <<\"This is a test\">>)">>,
    {ok, Source}.

'when'([joxa,is,called,on,this,module], Source, _) ->
    Result = 'joxa-compiler':forms(Source, []),
    {ok, Result}.

then([a,beam,binary,is,produced], Ctx, _) ->
    ?assertMatch(true, is_binary('joxa-compiler':'get-context'(result, Ctx))),
    {ok, Ctx};
then([the,described,function,can,be,called,'and',works,correctly], State, _) ->
    ?assertMatch([{'--joxa-info',1},
                  {'--joxa-info',2},
                  {'do-test1',0},
                  {'do-test2',0},
                  {'do-test3',0},
                  {'do-test4',0},
                  {module_info,0},
                  {module_info,1}],
                 lists:sort('jxat-binary-test':module_info(exports))),
    ?assertMatch({97,98,99}, 'jxat-binary-test':'do-test1'()),
    ?assertMatch({1,17,42}, 'jxat-binary-test':'do-test2'()),
    ?assertMatch({273,0,<<"*">>}, 'jxat-binary-test':'do-test3'()),
    ?assertMatch(<<"This is a test">>, 'jxat-binary-test':'do-test4'()),

    {ok, State}.


