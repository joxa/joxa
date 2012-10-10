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
    {ok, Ctx} = 'joxa-cmp-ctx':'start-context'([{attrs, [{foo, bar}]},
                                                 {annots, ec_dictionary:new(ec_dict)}]),
    ?assertMatch([{foo, bar}],
                 'joxa-cmp-ctx':'get-context'(attrs,
                                               'joxa-cmp-ctx':'get-raw-context'(Ctx))),
    'joxa-cmp-ctx':'add-export-ctx'(Ctx, 10, super, 3),
    ?assertMatch([{super, 3, 10}],
                 sets:to_list('joxa-cmp-ctx':'get-context'(exports,
                                                            'joxa-cmp-ctx':'get-raw-context'(Ctx)))),
    'joxa-cmp-ctx':'add-require-ctx'(Ctx, filelib),

    ?assert(ec_dictionary:has_key({filelib,file_size,2},
                                  'joxa-cmp-ctx':'get-context'(requires,
                                                                'joxa-cmp-ctx':'get-raw-context'(Ctx)))),
    'joxa-cmp-ctx':'add-use-ctx'(Ctx, print, 2, format, io),

    ?assertMatch([{{print, 2}, {format, io}}],
                 ec_dictionary:to_list(
                   'joxa-cmp-ctx':'get-context'(uses,
                                                 'joxa-cmp-ctx':'get-raw-context'(Ctx)))),
    'joxa-cmp-ctx':'push-scope-ctx'(Ctx),
    'joxa-cmp-ctx':'add-def-ctx'({0, [1]}, Ctx, [], foo, [], none),
    'joxa-cmp-ctx':'add-def-ctx'({0, [1]}, Ctx, [], foo, [one, two], none),
    'joxa-cmp-ctx':'add-alias-ctx'(Ctx, bar, lists),
    'joxa-cmp-ctx':'add-require-ctx'(Ctx, lists),
    'joxa-cmp-ctx':'add-reference-to-scope-ctx'({1, [1]}, Ctx, foo, -1,
                                                 cerl:c_var(foo)),

    ?assertMatch({reference, {{c_var, [], foo}, -1}},
                 'joxa-cmp-ctx':'resolve-reference-ctx'({0, [1]}, Ctx, foo, -1)),

    ?assertMatch({apply, 'not-rest', 'not-macro', {foo, 2}},
                 'joxa-cmp-ctx':'resolve-reference-ctx'({0, [1]}, Ctx, foo, 2)),

    ?assertMatch('not-a-reference',
                 'joxa-cmp-ctx':'resolve-reference-ctx'({0, [1]}, Ctx, foo, 3)),

    ?assertMatch('not-a-reference',
                 'joxa-cmp-ctx':'resolve-reference-ctx'({0, [1]}, Ctx, foo, 1)),

    ?assertMatch({apply, 'not-rest', 'not-macro', {foo, 0}},
                 'joxa-cmp-ctx':'resolve-reference-ctx'({0, [1]}, Ctx, foo, 0)),

    ?assertMatch({remote, 'not-rest', 'not-macro', {io, format, 2}},
                 'joxa-cmp-ctx':'resolve-reference-ctx'({0, [1]}, Ctx, print, 2)),

    ?assertMatch({remote, 'not-rest', 'not-macro', {lists, zipwith3, 4}},
                 'joxa-cmp-ctx':'resolve-reference-ctx'({0, [1]}, Ctx, {'--fun', lists,
                                                                         zipwith3, 4}, 4)),

    ?assertMatch({remote, 'not-rest', 'not-macro', {lists, zipwith3, 4}},
                 'joxa-cmp-ctx':'resolve-reference-ctx'({0, [1]}, Ctx, {'--fun', bar,
                                                                         zipwith3, 4}, 4)),

    ?assertMatch('not-a-reference',
                 'joxa-cmp-ctx':'resolve-reference-ctx'({0, [1]}, Ctx, {'--fun', bar,
                                                                         zipwith3, 3}, 4)),
    'joxa-cmp-ctx':'stop-context'(Ctx).
