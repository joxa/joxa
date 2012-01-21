-module(jxat_let_support).

-export([given/3, 'when'/3, then/3]).

-include_lib("eunit/include/eunit.hrl").

given([a,module,that,has,a,function,that,contains,'\'let\''], _State, _) ->
   Source = <<"(module jxat-let-support-test
                    (require erlang)
                    (use (io :only (format/2) :rename ((format/2 print)))))

                (defn internal-test ()
                      [1 2 3 4 5 6 7 8])

                (defn+ do-test ()
                      (let (foo 1
                            z (erlang/phash2 22)
                            a 1
                            a1 (internal-test)
                            bar [1 2 3 4 5] ; This is a comment test
                            baz \"Hello World\"
                            bad-boy 'super-dooper)
                           (do
                              ; as is this
                              (print \"~i ~p ~s ~p ~p~n\"
                                 [foo bar baz bad-boy (1 . 2)])
                              {foo bar baz bad-boy z a a1})))">>,

    {ok, Source}.

'when'([joxa,is,called,on,this,module], Source, _) ->
    Result = joxa.compiler:forms("", Source, []),
    {ok, Result}.


then([a,beam,binary,is,produced], State={_, Binary}, _) ->
    ?assertMatch(true, is_binary(Binary)),
    {ok, State};
then([the,described,function,can,be,called,'and',works,correctly], State, _) ->
    ?assertMatch([{'do-test',0},
                  {module_info,0},
                  {module_info,1}],
                 lists:sort('jxat-let-support-test':module_info(exports))),
    ?assertMatch({1,[1,2,3,4,5], "Hello World",
                  'super-dooper',73439361,1,[1,2,3,4,5,6,7,8]},
                 'jxat-let-support-test':'do-test'()),
    {ok, State}.

