%%%-------------------------------------------------------------------
%%% @author Eric B Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2011, Eric B Merritt
%%% @doc
%%%
%%% @end
%%% Created : 29 Dec 2011 by Eric B Merritt <ericbmerritt@gmail.com>
%%%-------------------------------------------------------------------
-module(jxat_parse).

%% API
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================
test1_test() ->
    Source = <<"(ns jxat-anon-fun
                    (use (erlang :only (==/2 phash2/1))))

                (defn internal-test ()
                      (fn (arg1 arg2)
                        {:hello arg1 arg2}))

                (defn+ do-test ()
                      (let (z (internal-test)
                            x (fn (arg1 arg2)
                                {:hello arg1 arg2})
                            a erlang/phash2/1
                            y (x 1 2))
                           (do (== y (x 1 2))
                               (apply internal-test/0)
                                (apply x 1 2)
                                (apply a 22)
                                (a 22))))">>,
    {ok, Ctx} = joxa.compiler:'start-context'(),
    {AST0, Remainder0} = joxa.compiler:parse(Ctx, Source),
    Annots0 = joxa.compiler:'annots-ctx'(Ctx),
    test_type_idx([1,1], ident, 1, Annots0),
    test_type_idx([2,1], ident, 1, Annots0),
    test_type_idx([3,1], list, 2, Annots0),
    test_type_idx([2,3,1], list, 2, Annots0),

    ?assertMatch([ns,'jxat-anon-fun',
                  [use,[erlang,[quote,only],[{'--fun','==',2},
                                             {'--fun',phash2,1}]]]],
                 AST0),


    {AST1, Remainder1} = joxa.compiler:parse(Ctx, Remainder0),
    Annots1 = joxa.compiler:'annots-ctx'(Ctx),
    test_type_idx([1,2], ident, 4, Annots1),
    test_type_idx([2,2], ident, 4, Annots1),
    test_type_idx([3,2], list, 4, Annots1),
    test_type_idx([4,2], list, 5, Annots1),
    test_type_idx([2,4,2], list, 5, Annots1),

    ?assertMatch([defn,'internal-test',[],
                  [fn,[arg1,arg2],{[quote,hello],arg1,arg2}]],
                 AST1),

    {AST2, _Remainder2} = joxa.compiler:parse(Ctx, Remainder1),
    Annots2 = joxa.compiler:'annots-ctx'(Ctx),
    test_type_idx([1,3], ident, 8, Annots2),
    test_type_idx([2,3], ident, 8, Annots2),
    test_type_idx([3,3], list, 8, Annots2),
    test_type_idx([4,3], list, 9, Annots2),
    test_type_idx([2,4,3], list, 9, Annots2),
    test_type_idx([1,3,4,3], ident, 14, Annots2),

    ?assertMatch(
       ['defn+','do-test',[],
        ['let',
         [z,['internal-test'],
          x,[fn,[arg1,arg2],{[quote,hello],arg1,arg2}],
          a,{'--fun',erlang,phash2,1},
          y,[x,1,2]],
         [do,
          ['==',y,[x,1,2]],
          [apply,{'--fun','internal-test',0}],
          [apply,x,1,2],
          [apply,a,22],
          [a,22]]]],
       AST2),
    ok = joxa.compiler:'stop-context'(Ctx).

%%%===================================================================
%%% Support Functions
%%%===================================================================

test_type_idx(Path, Type, Line, Annots) ->
    ?assertMatch(Type, joxa.compiler:'get-type-annots'(Path, Annots)),
    ?assertMatch({Line, _}, joxa.compiler:'get-idx-annots'(Path, Annots)).
