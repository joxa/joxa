%% -*- mode: Erlang; fill-column: 80; comment-column: 76; -*-
-module(jxa_let).

-export([comp/3]).
-include_lib("joxa/include/joxa.hrl").

%%=============================================================================
%% Public API
%%=============================================================================
comp(Path0, Ctx0, ['let', Bindings, Body])
  when is_list(Bindings) ->
    Path1 = jxa_path:add(jxa_path:incr(Path0)),
    compile_bindings(jxa_path:incr(2, Path0), Path1, Ctx0, Bindings, Body);
comp(Path1, Ctx0, _) ->
    Idx = jxa_annot:get_idx(jxa_path:path(Path1), jxa_ctx:annots(Ctx0)),
    ?JXA_THROW({invalid_form, Idx}).

%%=============================================================================
%% Private API
%%=============================================================================
compile_bindings(Path0, Path1, Ctx0, [[Var, Expr]], Body) ->
    compile_binding(Path0, Path1, Ctx0, Var, Expr,
                    fun(Path0a, _Path1a, Ctx1) ->
                            jxa_expression:comp(jxa_path:add(Path0a), Ctx1,
                                                Body)

                    end);
compile_bindings(Path0, Path1, Ctx0, [[Var, Expr] | Rest], Body) ->
    compile_binding(Path0, Path1, Ctx0, Var, Expr,
                    fun(Path0a, Path1a, Ctx1) ->
                            compile_bindings(Path0a,
                                             jxa_path:incr(Path1a),
                                             Ctx1, Rest, Body)
                    end);
compile_bindings(_Path0, Path1, Ctx0, _, _Body) ->
    Idx =
        jxa_annot:get_idx(jxa_path:add_path(Path1),
                          jxa_ctx:annots(Ctx0)),
    ?JXA_THROW({invalid_let_binding,  Idx}).

compile_binding(Path0, Path1, Ctx0, Var, Expr=[fn, Args, _],
                Continuation) when not is_list(Var) ->
    %% For letrecs the variable has to be available in the scope so
    %% the function can call as needed
    Arity = erlang:length(Args),
    Ctx1 = jxa_ctx:add_variable_to_scope(Var, Arity,
                                         jxa_ctx:push_scope(Ctx0)),
    Annots = jxa_annot:get_line(jxa_path:add_path(Path0),
                                jxa_ctx:annots(Ctx0)),
    CerlVar = cerl:ann_c_fname(Annots, Var, Arity),
    {Ctx3, CerlExpr} =
        jxa_expression:comp(jxa_path:add(jxa_path:incr(jxa_path:add(Path1))),
                            Ctx1, Expr),

    {Ctx4, CerlBody} =
        Continuation(Path0, Path1, Ctx3),
    {jxa_ctx:pop_scope(Ctx4),
     cerl:ann_c_letrec(Annots, [{CerlVar, CerlExpr}],
                       CerlBody)};
compile_binding(Path0, Path1, Ctx0, Var, Expr, Continuation) ->
    {Ctx1, CerlExpr} =
        jxa_expression:comp(jxa_path:add(jxa_path:incr(jxa_path:add(Path1))),
                            Ctx0, Expr),
    Ctx2 = jxa_ctx:push_scope(Ctx1),
    {Ctx3, CerlVar} = comp_vars(jxa_path:add(Path1), Ctx2, Var),
    Annots = jxa_annot:get_line(jxa_path:add_path(Path1),
                                jxa_ctx:annots(Ctx3)),
    {Ctx4, CerlBody} =
        Continuation(Path0, Path1, Ctx3),

    {jxa_ctx:pop_scope(Ctx4), cerl:ann_c_let(Annots, CerlVar, CerlExpr,
                                             CerlBody)}.

comp_vars(Path0, Ctx0, Var) when is_atom(Var) ->
    Ctx1 = jxa_ctx:add_variable_to_scope(Var, Ctx0),
    Annots = jxa_annot:get_line(jxa_path:add_path(Path0),
                                jxa_ctx:annots(Ctx0)),
    {Ctx1, [cerl:ann_c_var(Annots, Var)]};
comp_vars(Path0, Ctx0, Vars0) when is_list(Vars0) ->
    {_, Ctx3, Vars1} =
        lists:foldl(fun(Var, {Path1, Ctx1, Acc}) when is_atom(Var) ->
                            Ctx2 = jxa_ctx:add_variable_to_scope(Var, Ctx1),
                            Annots =
                                jxa_annot:get_line(jxa_path:add_path(Path1),
                                                   jxa_ctx:annots(Ctx1)),
                            {Path1, Ctx2,
                             [cerl:ann_c_var(Annots, Var) | Acc]};
                       (Var, {Path1, Ctx1, _Acc}) ->
                            Idx =
                                jxa_annot:get_idx(jxa_path:add_path(Path1),
                                                  jxa_ctx:annots(Ctx1)),
                            ?JXA_THROW({invalid_variable_name,  Var, Idx})
                    end, {Path0, Ctx0, []}, Vars0),
    {Ctx3, lists:reverse(Vars1)}.


