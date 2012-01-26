-module(jxat_rest_args).

-export([given/3, 'when'/3, then/3]).
-include_lib("eunit/include/eunit.hrl").

given([a,module,that,has,rest,arguments], _State, _) ->
     Source1 = <<"(module jxat-rest-test1
                    (require erlang io))

                  (defn+ do-test0 (one &rest two)
                       {one two})

                 (defn+ do-test1 ()
                       do-test0/2)

                 (defn+ do-test2 ()
                       do-test0/5)
                 ">>,

     Source2 = <<"(module jxat-rest-test2
                    (require erlang io jxat-rest-test1))


                 (defn+ do-test0()
                    (jxat-rest-test1/do-test0 1 2 3 4 5 6 7 9 10))

                (defn internal-test0 (one &rest two)
                    {one two})

                 (defn+ do-test1()
                    (internal-test0 1 2 3 4 5 6 7 9 10))

                 (defn+ do-test2()
                     jxat-rest-test1/do-test0/2)

                 (defn+ do-test3()
                     jxat-rest-test1/do-test0/5)">>,

    {ok, {Source1, Source2}}.

'when'([joxa,is,called,on,this,module], {Source1, Source2}, _) ->
    Result1 = joxa.compiler:forms("", Source1, []),
    Result2 = joxa.compiler:forms("", Source2, []),
   {ok, {Result1, Result2}}.

then([a,beam,binary,is,produced], State={{_, Binary1}, {_, Binary2}}, _) ->
    ?assertMatch(true, is_binary(Binary1)),
    ?assertMatch(true, is_binary(Binary2)),
    {ok, State};
then([the,described,function,can,be,called,'and',works,correctly], State, _) ->
    ?assertMatch(2, 'jxat-rest-test1':'--joxa-info'(rest, 'do-test0')),
    ?assertMatch({1,[2,3,4,5,6,7,9,10]}, 'jxat-rest-test2':'do-test1'()),
    ?assertMatch({1,[2]}, ('jxat-rest-test1':'do-test1'())(1, 2)),
    ?assertMatch({1,[2,3,4,5]}, ('jxat-rest-test1':'do-test2'())(1, 2, 3, 4, 5)),
    ?assertMatch({1,[2]}, ('jxat-rest-test2':'do-test2'())(1, 2)),
    ?assertMatch({1,[2,3,4,5]}, ('jxat-rest-test2':'do-test3'())(1, 2, 3, 4, 5)),

    {ok, State}.

