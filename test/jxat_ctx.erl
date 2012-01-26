%%%-------------------------------------------------------------------
%%% @author Eric B Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2011, Eric B Merritt
%%% @doc
%%%
%%% @end
%%% Created : 29 Dec 2011 by Eric B Merritt <ericbmerritt@gmail.com>
%%%-------------------------------------------------------------------
-module(jxat_ctx).

%% API
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================
test1_test() ->
    Ctx0 = joxa.compiler:'new-context'([{attrs, [{foo, bar}]}]),
    ?assertMatch([{foo, bar}],
                 joxa.compiler:'get-context'(attrs, Ctx0)),
    Ctx1 = joxa.compiler:'add-export-ctx'(10, super, 3, Ctx0),
    ?assertMatch([{super, 3, 10}],
                 sets:to_list(joxa.compiler:'get-context'(exports, Ctx1))),
    Ctx2 = joxa.compiler:'add-require-ctx'(filelib, Ctx1),
    ?assertMatch([{filelib, _}],
                 ec_dictionary:to_list(joxa.compiler:'get-context'(requires,
                                                                   Ctx2))),
    Ctx3 = joxa.compiler:'add-use-ctx'(print, 2, format, io, Ctx2),

    ?assertMatch([{{print, 2}, {format, io}}],
                 ec_dictionary:to_list(joxa.compiler:'get-context'(uses,
                                                                   Ctx3))),
    Ctx4 = joxa.compiler:'add-def-ctx'([], foo, [], none,
                                       joxa.compiler:'push-scope-ctx'(Ctx3)),
    Ctx5 = joxa.compiler:'add-def-ctx'([], foo, [one, two], none, Ctx4),
    Ctx6 = joxa.compiler:'add-require-ctx'(lists,
                                           joxa.compiler:'add-alias-ctx'(bar, lists, Ctx5)),
    Ctx7 = joxa.compiler:'add-reference-to-scope-ctx'(foo, -1, {1, [1]}, Ctx6),

    ?assertMatch({reference, {{c_var, [], foo}, -1}},
                 joxa.compiler:'resolve-reference-ctx'(foo, -1, Ctx7)),

    ?assertMatch({apply, {foo, 2}},
                 joxa.compiler:'resolve-reference-ctx'(foo, 2, Ctx7)),

    ?assertMatch('not-a-reference',
                 joxa.compiler:'resolve-reference-ctx'(foo, 3, Ctx7)),

    ?assertMatch('not-a-reference',
                 joxa.compiler:'resolve-reference-ctx'(foo, 1, Ctx7)),

    ?assertMatch({apply, {foo, 0}},
                 joxa.compiler:'resolve-reference-ctx'(foo, 0, Ctx7)),

    ?assertMatch({remote, {io, format, 2}},
                 joxa.compiler:'resolve-reference-ctx'(print, 2, Ctx7)),

    ?assertMatch({remote, {lists, zipwith3, 4}},
                 joxa.compiler:'resolve-reference-ctx'({'--fun', lists,
                                                        zipwith3, 4}, 4, Ctx7)),

    ?assertMatch({remote, {lists, zipwith3, 4}},
                 joxa.compiler:'resolve-reference-ctx'({'--fun', bar,
                                                        zipwith3, 4}, 4, Ctx7)),

    ?assertThrow({'mismatched-arity',bar,zipwith3,4,3},
                 joxa.compiler:'resolve-reference-ctx'({'--fun', bar,
                                                        zipwith3, 3}, 4, Ctx7)).
