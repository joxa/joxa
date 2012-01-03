%% -*- mode: Erlang; fill-column: 80; comment-column: 76; -*-
-module(jxa_try).

-export([comp/3]).
-include_lib("joxa/include/joxa.hrl").

%%=============================================================================
%% Public API
%%=============================================================================
comp(Path0, Ctx0, ['try', Expr, ['catch' | Clauses]]) ->
    Annots = jxa_annot:get_line(jxa_path:path(Path0),
                                jxa_ctx:annots(Ctx0)),
    {Ctx1, CerlExpr} = jxa_expression:comp(jxa_path:add(jxa_path:incr(Path0)),
                                           Ctx0,
                                           Expr),
    TryVar = cerl:ann_c_var([compiler_generated | Annots], joxa:gensym()),
    Type = cerl:ann_c_var([compiler_generated | Annots], joxa:gensym()),
    Value = cerl:ann_c_var([compiler_generated | Annots], joxa:gensym()),
    Ignored = cerl:ann_c_var([compiler_generated | Annots], joxa:gensym()),
    {Ctx2, CerlCase} =
        comp_case(jxa_path:incr(jxa_path:add(jxa_path:incr(2, Path0))),
                  Ctx1, Type, Value, Ignored, Clauses),
    {Ctx2,
     cerl:ann_c_try(Annots, CerlExpr,
                    [TryVar],
                    TryVar,
                    [Type, Value, Ignored],
                    CerlCase)};
comp(Path0, Ctx0, _) ->
    Idx = jxa_annot:get_idx(jxa_path:path(Path0),
                            jxa_ctx:annots(Ctx0)),
    ?JXA_THROW({invalid_try_expression, Idx}).



%%=============================================================================
%% Private API
%%=============================================================================
comp_case(Path0, Ctx0, TypeVar, ValueVar, Ignored, Clauses) ->
    Annots = jxa_annot:get_line(jxa_path:path(Path0),
                                compiler_generated,
                                jxa_ctx:annots(Ctx0)),

    Ctx1 =
        jxa_ctx:add_variable_to_scope(ValueVar,
                                      jxa_ctx:add_variable_to_scope(TypeVar, Ctx0)),
    {_, Ctx4, CerlClauses0} =
        lists:foldl(fun(El, {Path1, Ctx2, Acc}) ->
                            {Ctx3, Clause} =
                                comp_clause(jxa_path:add(Path1), Ctx2, El),
                            {jxa_path:incr(Path1),
                             Ctx3, [Clause | Acc]}
                    end, {Path0, Ctx1, []}, Clauses),

    CerlClauses1 = [generate_reraise_clause(TypeVar, ValueVar, Ignored) |
                    CerlClauses0],
    Values =
        cerl:ann_c_values(Annots,
                          [TypeVar, ValueVar, Ignored]),
    {Ctx4,
     cerl:ann_c_case(Annots,
                     Values,
                     lists:reverse(CerlClauses1))}.

comp_clause(Path0, Ctx0, [Type, Pattern, Body]) ->
    Annots = jxa_annot:get_line(jxa_path:path(Path0),
                                compiler_generated,
                                jxa_ctx:annots(Ctx0)),
    Ctx1 = jxa_ctx:push_scope(Ctx0),
    {{Ctx2, TypeGuards}, CerlType} =
        jxa_clause:comp_pattern(jxa_path:add(Path0), {Ctx1, []}, Type),
    {{Ctx3, PatternGuards}, CerlPattern} =
        jxa_clause:comp_pattern(jxa_path:add(jxa_path:incr(Path0)),
                                {Ctx2, TypeGuards},
                                Pattern),

    {Ctx4, CerlBody} =
        jxa_expression:comp(jxa_path:add(jxa_path:incr(2, Path0)),
                            Ctx3, Body),
    {jxa_ctx:pop_scope(Ctx4),
     cerl:ann_c_clause(Annots,
                       [CerlType, CerlPattern,
                        cerl:ann_c_var([compiler_generated], '_')],
                       jxa_clause:mk_guards(Annots, PatternGuards),
                       CerlBody)};
comp_clause(Path0, Ctx0, [Type, Pattern, ['when', Guards], Body]) ->
    Annots = jxa_annot:get_line(jxa_path:path(Path0),
                                jxa_ctx:annots(Ctx0)),
    Ctx1 = jxa_ctx:push_scope(Ctx0),
    {{Ctx2, TypeGuards}, CerlType} =
        jxa_clause:comp_pattern(jxa_path:add(Path0), {Ctx1, []}, Type),
    {{Ctx3, PatternGuards}, CerlPattern} =
        jxa_clause:comp_pattern(jxa_path:add(jxa_path:incr(Path0)),
                                {Ctx2, TypeGuards},
                                Pattern),
    {Ctx4, CerlGuard} =
        jxa_expression:comp(
          jxa_path:add(jxa_path:incr(
                         jxa_path:add(jxa_path:incr(Path0)))),
          Ctx3, Guards),

    {Ctx5, CerlBody} =
        jxa_expression:comp(jxa_path:add(jxa_path:incr(2, Path0)),
                            Ctx4, Body),
    {jxa_ctx:pop_scope(Ctx5),
     cerl:ann_c_clause(Annots,
                       [CerlType, CerlPattern,
                        cerl:ann_c_var([compiler_generated], '_')],
                       jxa_clause:mk_guards(Annots, [CerlGuard, PatternGuards]),
                       CerlBody)};

comp_clause(Path0, Ctx0, _) ->
    Idx = jxa_annot:get_idx(jxa_path:path(Path0),
                            jxa_ctx:annots(Ctx0)),
    ?JXA_THROW({invalid_catch_clause, Idx}).


generate_reraise_clause(Type, Value, Ignored) ->
    Annots = [compiler_generated],
    cerl:ann_c_clause(Annots,
                      [Type, Value, Ignored],
                      cerl:ann_c_atom(Annots, true),
                      cerl:ann_c_primop(Annots,
                                        cerl:ann_c_atom(Annots, raise),
                                        [Type, Value])).
