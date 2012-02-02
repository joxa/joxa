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
    {ok, Ctx} = joxa.compiler:'start-context'([{attrs, [{foo, bar}]},
                                               {annots, ec_dictionary:new(ec_dict)}]),
    ?assertMatch([{foo, bar}],
                 joxa.compiler:'internal-get-context'(attrs,
                                                      joxa.compiler:'get-raw-context'(Ctx))),
    joxa.compiler:'add-export-ctx'(Ctx, 10, super, 3),
    ?assertMatch([{super, 3, 10}],
                 sets:to_list(joxa.compiler:'internal-get-context'(exports,
                                                                  joxa.compiler:'get-raw-context'(Ctx)))),
    joxa.compiler:'add-require-ctx'(Ctx, filelib),

    ?assertMatch([{filelib, _}],
                 ec_dictionary:to_list(
                   joxa.compiler:'internal-get-context'(requires,
                                                        joxa.compiler:'get-raw-context'(Ctx)))),
    joxa.compiler:'add-use-ctx'(Ctx, print, 2, format, io),

    ?assertMatch([{{print, 2}, {format, io}}],
                 ec_dictionary:to_list(
                   joxa.compiler:'internal-get-context'(uses,
                                                        joxa.compiler:'get-raw-context'(Ctx)))),
    joxa.compiler:'push-scope-ctx'(Ctx),
    joxa.compiler:'add-def-ctx'({0, [1]}, Ctx, [], foo, [], none),
    joxa.compiler:'add-def-ctx'({0, [1]}, Ctx, [], foo, [one, two], none),
    joxa.compiler:'add-alias-ctx'(Ctx, bar, lists),
    joxa.compiler:'add-require-ctx'(Ctx, lists),
    joxa.compiler:'add-reference-to-scope-ctx'({1, [1]}, Ctx, foo, -1,
                                               cerl:c_var(foo)),

    ?assertMatch({reference, {{c_var, [], foo}, -1}},
                 joxa.compiler:'resolve-reference-ctx'({0, [1]}, Ctx, foo, -1)),

    ?assertMatch({apply, 'not-rest', 'not-macro', {foo, 2}},
                 joxa.compiler:'resolve-reference-ctx'({0, [1]}, Ctx, foo, 2)),

    ?assertMatch('not-a-reference',
                 joxa.compiler:'resolve-reference-ctx'({0, [1]}, Ctx, foo, 3)),

    ?assertMatch('not-a-reference',
                 joxa.compiler:'resolve-reference-ctx'({0, [1]}, Ctx, foo, 1)),

    ?assertMatch({apply, 'not-rest', 'not-macro', {foo, 0}},
                 joxa.compiler:'resolve-reference-ctx'({0, [1]}, Ctx, foo, 0)),

    ?assertMatch({remote, 'not-rest', 'not-macro', {io, format, 2}},
                 joxa.compiler:'resolve-reference-ctx'({0, [1]}, Ctx, print, 2)),

    ?assertMatch({remote, 'not-rest', 'not-macro', {lists, zipwith3, 4}},
                 joxa.compiler:'resolve-reference-ctx'({0, [1]}, Ctx, {'--fun', lists,
                                                             zipwith3, 4}, 4)),

    ?assertMatch({remote, 'not-rest', 'not-macro', {lists, zipwith3, 4}},
                 joxa.compiler:'resolve-reference-ctx'({0, [1]}, Ctx, {'--fun', bar,
                                                             zipwith3, 4}, 4)),

    ?assertMatch('not-a-reference',
                 joxa.compiler:'resolve-reference-ctx'({0, [1]}, Ctx, {'--fun', bar,
                                                             zipwith3, 3}, 4)),
    joxa.compiler:'stop-context'(Ctx).
