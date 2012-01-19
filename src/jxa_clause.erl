%% -*- mode: Erlang; fill-column: 80; comment-column: 76; -*-
-module(jxa_clause).

-export([comp/3, comp_pattern/3, mk_guards/2]).
-include_lib("joxa/include/joxa.hrl").

-define(UNDERSCORED, {re_pattern,0,0,
                      <<69,82,67,80,61,0,0,0,16,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,48,0,0,0,
                        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,93,0,9,25,27,95,56,12,
                        26,84,0,9,0>>}).

%%=============================================================================
%% Public API
%%=============================================================================
comp(Path0, Ctx0, Clauses) ->
    comp(Path0, Ctx0, Clauses, []).

comp_pattern(Path0, Acc0={Ctx0, _}, Arg) when is_binary(Arg) ->
    Annots = jxa_annot:get_line(jxa_path:path(Path0),
                                jxa_ctx:annots(Ctx0)),
    {Acc0, cerl:ann_make_data(Annots, {atomic, Arg}, [])};
comp_pattern(Path0, {Ctx0, Guards0}, '_') ->
    Annots = jxa_annot:get_line(jxa_path:path(Path0),
                                jxa_ctx:annots(Ctx0)),
    Gensym = erlang:list_to_atom("_" ++ erlang:atom_to_list(joxa:gensym())),

    CerlVar = cerl:ann_c_var(Annots, Gensym),
    Ctx1 = jxa_ctx:add_variable_to_scope(Gensym, Ctx0),
    {{Ctx1, Guards0}, CerlVar};
comp_pattern(Path0, {Ctx0, Guards0}, Arg) when is_atom(Arg) ->
    Annots = jxa_annot:get_line(jxa_path:path(Path0),
                                compiler_generated,
                                jxa_ctx:annots(Ctx0)),
    case jxa_ctx:resolve_reference(Arg, -1, Ctx0) of
        {variable, Var} ->
            %% The reference already exists. So we create a new variable and
            %% add a guard for to test for equality
            GenSym = joxa:gensym(),
            CerlVar = cerl:ann_c_var(Annots, GenSym),
            Guards1 = [cerl:ann_c_call(Annots,
                                       cerl:ann_c_atom([compiler_generated],
                                                       'erlang'),
                                       cerl:ann_c_atom(Annots,
                                                       '=:='),
                                       [CerlVar, cerl:set_ann(Var, Annots)])
                       | Guards0],
            %% We don't add the generated variable to the scope as we
            %% don't want it to be available to the user (The user really
            %% should not even be aware of it
            {{Ctx0, Guards1}, CerlVar};
        _ ->
            AnnotsBare = jxa_annot:get_line(jxa_path:path(Path0),
                                            jxa_ctx:annots(Ctx0)),
            %% The variable is not in the scope so we turn it to a
            %% variable
            CerlVar = cerl:ann_c_var(AnnotsBare, Arg),
            Ctx1 = jxa_ctx:add_variable_to_scope(Arg, Ctx0),
            {{Ctx1, Guards0}, CerlVar}
    end;
comp_pattern(Path0, Acc0={Ctx0, _}, Arg) when is_integer(Arg) ->
    Annots = jxa_annot:get_line(jxa_path:path(Path0),
                                jxa_ctx:annots(Ctx0)),
    {Acc0, cerl:ann_c_int(Annots, Arg)};
comp_pattern(Path0, Acc0={Ctx0, _}, Arg) when is_float(Arg) ->
    Annots = jxa_annot:get_line(jxa_path:path(Path0),
                                jxa_ctx:annots(Ctx0)),
    {Acc0, cerl:ann_c_float(Annots, Arg)};
comp_pattern(Path0, Acc0={Ctx0, _}, ['=', Arg1, Arg2])
  when is_atom(Arg1) ->
    Annots = jxa_annot:get_line(jxa_path:path(Path0),
                                jxa_ctx:annots(Ctx0)),
    Arg1Annots = jxa_annot:get_line(jxa_path:path(jxa_path:incr(Path0)),
                                    jxa_ctx:annots(Ctx0)),
    {{Ctx1, Guards}, CerlPattern} =
        comp_pattern(jxa_path:add(jxa_path:incr(2, Path0)),
                     Acc0, Arg2),
    CerlArg1 =
        cerl:ann_c_alias(Annots, cerl:ann_c_var(Arg1Annots, Arg1),
                         CerlPattern),
    {{jxa_ctx:add_variable_to_scope(Arg1, Ctx1), Guards}, CerlArg1};
comp_pattern(Path0, Acc0={Ctx0, _}, ['=', Arg1, Arg2])
  when is_atom(Arg2) ->
    Annots = jxa_annot:get_line(jxa_path:path(Path0),
                                jxa_ctx:annots(Ctx0)),
    Arg2Annots = jxa_annot:get_line(jxa_path:path(jxa_path:incr(Path0)),
                                    jxa_ctx:annots(Ctx0)),
    {{Ctx1, Guards}, CerlPattern} =
        comp_pattern(jxa_path:add(jxa_path:incr(Path0)),
                     Acc0, Arg1),
    CerlArg1 =
        cerl:ann_c_alias(Annots, cerl:ann_c_var(Arg2Annots, Arg2),
                         CerlPattern),
    {{jxa_ctx:add_variable_to_scope(Arg2, Ctx1), Guards}, CerlArg1};
comp_pattern(Path0, Acc0={Ctx0, _}, [quote, Args]) ->
    Literal = jxa_literal:comp(jxa_path:add(jxa_path:incr(Path0)),
                               Ctx0, Args),
    {Acc0, Literal};
comp_pattern(Path0, Acc0, Expr=[binary | _]) ->
    jxa_binary:comp_pattern(Path0, Acc0, Expr);
comp_pattern(Path0, Acc0, [Arg1, '.', Arg2]) ->
    {Acc1, Cerl1} = comp_pattern(jxa_path:add(Path0),
                                 Acc0, Arg1),
    {Acc2={Ctx1, _}, Cerl2} = comp_pattern(jxa_path:add(jxa_path:incr(2, Path0)),
                                           Acc1, Arg2),
    Annots = jxa_annot:get_line(jxa_path:add_path(Path0),
                                jxa_ctx:annots(Ctx1)),
    {Acc2, cerl:ann_c_cons(Annots, Cerl1, Cerl2)};
comp_pattern(Path0, Acc0, [cons, Arg1, Arg2]) ->
    {Acc1, Cerl1} = comp_pattern(jxa_path:incr(Path0),
                                 Acc0, Arg1),
    {Acc2={Ctx0, _}, Cerl2} = comp(jxa_path:incr(2, Path0),
                                   Acc1, Arg2),
    Annots = jxa_annot:get_line(jxa_path:add_path(Path0),
                                jxa_ctx:annots(Ctx0)),
    {Acc2, cerl:ann_c_cons(Annots, Cerl1, Cerl2)};
comp_pattern(Path0, Acc0={Ctx0, _}, [quote, Args]) ->
    Literal = jxa_literal:comp(jxa_path:incr(Path0), Ctx0, Args),
    {Acc0, Literal};
comp_pattern(Path0, Acc0, [list | Args]) ->
    Path1 = jxa_path:incr(Path0),
    mk_list(Path1, Acc0, Args);
comp_pattern(Path0, Acc0, [tuple | Args]) ->
    mk_tuple(Path0, Acc0, Args);
comp_pattern(Path0, Acc0, Arg) when is_tuple(Arg) ->
    mk_tuple(Path0, Acc0, tuple_to_list(Arg));
comp_pattern(Path0, {Ctx0, _}, _Arg) ->
    Idx = jxa_annot:get_idx(jxa_path:path(Path0),
                            jxa_ctx:annots(Ctx0)),
    ?JXA_THROW({invalid_pattern, Idx}).

mk_guards(GuardAnnots, []) ->
    cerl:ann_c_atom(GuardAnnots, true);
mk_guards(_GuardAnnots, [Guard]) ->
    Guard;
mk_guards(GuardAnnots, [Pattern | Rest]) ->
    cerl:ann_c_call(GuardAnnots,
                    cerl:ann_c_atom(GuardAnnots,
                                    'erlang'),
                    cerl:ann_c_atom(GuardAnnots,
                                    'and'),
                    [Pattern, mk_guards(GuardAnnots, Rest)]).

%%=============================================================================
%% Private API
%%=============================================================================
comp(Path0, Ctx0, [], _Acc) ->
    Idx = jxa_annot:get_idx(jxa_path:path(Path0),
                            jxa_ctx:annots(Ctx0)),
    ?JXA_THROW({no_clauses_provided, Idx});
comp(Path0, Ctx0, [Clause = [Pattern | _Rest]], Acc) when is_atom(Pattern) ->
    %% When there is one clause left in the list of clauses (the last clause) If
    %% it is not a catchall we want to make a catchall. Simplistically at the
    %% moment we look for a single variable name that is unbound that starts
    %% with an '_'
    {Ctx1, ActualClause} =
        compile_clause_body(jxa_path:add(Path0), Ctx0, Clause),
    {Ctx1, lists:reverse([ActualClause | Acc])};
comp(Path0, Ctx0, [Clause], Acc) ->
    {Ctx1, Clauses, SpecialTerminator} =
        do_clause_terminator(Path0, Ctx0, Clause),
    {Ctx1, lists:reverse([Clauses | Acc]) ++ SpecialTerminator};
comp(Path0, Ctx0, [Clause | Rest], Acc) ->
    {Ctx1, Clauses} = compile_clause_body(jxa_path:add(Path0),
                                          Ctx0,
                                          Clause),
    comp(jxa_path:incr(Path0), Ctx1, Rest, [Clauses | Acc]).


do_clause_terminator(Path0, Ctx0, Clause) ->
    {Ctx1, ActualClause} =
        compile_clause_body(jxa_path:add(Path0), Ctx0,
                            Clause),
    Annots = jxa_annot:get_line(jxa_path:path(Path0),
                                compiler_generated,
                                jxa_ctx:annots(Ctx1)),
    Var = joxa:gensym(),
    {Ctx1,
     ActualClause,
     [cerl:ann_c_clause(Annots,
                        [cerl:ann_c_var(Annots, Var)],
                        cerl:ann_c_primop(Annots,
                                          cerl:ann_c_atom(Annots, 'match_fail'),
                                          [cerl:ann_c_tuple(Annots,
                                                            [cerl:ann_c_atom(Annots, case_clause),
                                                             cerl:ann_c_var(Annots, Var)])]))]}.



compile_clause_body(Path0, Ctx0, [Pattern, ['when', Guards], Body]) ->
    Ctx1 = jxa_ctx:push_scope(Ctx0),
    Annots = jxa_annot:get_line(jxa_path:path(Path0),
                                jxa_ctx:annots(Ctx1)),
    {{Ctx2, PatternGuards}, CerlPattern} =
        comp_pattern(jxa_path:add(Path0), {Ctx1, []}, Pattern),
    GuardAnnots =
        jxa_annot:get_line(jxa_path:add_path(jxa_path:incr(Path0)),
                           jxa_ctx:annots(Ctx2)),
    Idx =
        jxa_annot:get_idx(jxa_path:add_path(jxa_path:incr(Path0)),
                           jxa_ctx:annots(Ctx2)),
    {Ctx3, CerlGuard} =
        jxa_expression:comp(
          jxa_path:add(jxa_path:incr(jxa_path:add(jxa_path:incr(Path0)))),
          Ctx2, Guards),
    CompleteGuards = check_guards(Idx, mk_guards(GuardAnnots,
                                                 [CerlGuard | PatternGuards])),

    {Ctx4, CerlBody} =
        jxa_expression:comp(jxa_path:add(jxa_path:incr(2, Path0)),
                            Ctx3, Body),
    {jxa_ctx:pop_scope(Ctx4),
     cerl:ann_c_clause(Annots, [CerlPattern], CompleteGuards,
                       CerlBody)};
compile_clause_body(Path0, Ctx0, [Pattern, Body]) ->
    Ctx1 = jxa_ctx:push_scope(Ctx0),
    Annots = jxa_annot:get_line(jxa_path:path(Path0),
                                jxa_ctx:annots(Ctx1)),
    {{Ctx2, PatternGuards}, CerlPattern} =
        comp_pattern(jxa_path:add(Path0), {Ctx1, []}, Pattern),
    {Ctx3, CerlBody} =
        jxa_expression:comp(jxa_path:add(jxa_path:incr(Path0)),
                            Ctx2, Body),
    CompleteGuards = mk_guards(Annots, PatternGuards),

    {jxa_ctx:pop_scope(Ctx3),
     cerl:ann_c_clause(Annots, [CerlPattern],
                       CompleteGuards,
                       CerlBody)};
compile_clause_body(Path0, Ctx0, _) ->
    Idx = jxa_annot:get_idx(jxa_path:path(Path0),
                            jxa_ctx:annots(Ctx0)),
    ?JXA_THROW({invalid_case_clause, Idx}).


mk_tuple(Path0, Acc0, Args) ->
    {_, Acc3={Ctx0, _}, Body} =
        lists:foldl(fun(Arg, {Path2, Acc1, LAcc}) ->
                            {Acc2, Element} =
                                comp_pattern(jxa_path:add(Path2), Acc1, Arg),
                            Path3 = jxa_path:incr(Path2),
                            {Path3, Acc2, [Element | LAcc]}
                    end, {Path0, Acc0, []}, Args),
    Annots = jxa_annot:get_line(jxa_path:path(Path0),
                                jxa_ctx:annots(Ctx0)),
    {Acc3, cerl:ann_c_tuple(Annots, lists:reverse(Body))}.

mk_list(_Path0, Acc0, []) ->
    {Acc0, cerl:c_nil()};
mk_list(Path0, Acc0, [H | T]) ->
    {Acc1, CerlH} = comp_pattern(jxa_path:add(Path0),
                                 Acc0, H),
    {Acc2={Ctx0, _}, CerlT} = mk_list(jxa_path:incr(Path0),
                                      Acc1, T),
    Annots = jxa_annot:get_line(
               jxa_path:add_path(Path0),
               jxa_ctx:annots(Ctx0)),
    {Acc2, cerl:ann_c_cons(Annots, CerlH, CerlT)}.

check_guards(AST) ->
    case cerl:type(AST) of
        call ->
            valid_guard(cerl:atom_val(cerl:call_module(AST)),
                        cerl:atom_val(cerl:call_name(AST)),
                        cerl:call_arity(AST)) andalso
                lists:all(fun check_guards/1, cerl:call_args(AST));
        cons ->
            check_guards(cerl:cons_head(AST)) andalso
                check_guards(cerl:cons_tail(AST));
        binary ->
            true;
        bitstring ->
            true;
        tuple ->
            lists:all(fun check_guards/1, cerl:tuple_es(AST));
        var ->
            true;
        literal ->
            true;
        _ ->
            false
    end.

check_guards(Idx, AST) ->
    case check_guards(AST) of
        true ->
            AST;
        false ->
            ?JXA_THROW({invalid_guard, Idx})
    end.

valid_guard(erlang, abs, 1) -> true;
valid_guard(erlang, bitsize, 1) -> true;
valid_guard(erlang, byte_size, 1) -> true;
valid_guard(erlang, element, 2) -> true;
valid_guard(erlang, float, 1) -> true;
valid_guard(erlang, hd, 1) -> true;
valid_guard(erlang, length, 1) -> true;
valid_guard(erlang, node, 0) -> true;
valid_guard(erlang, node, 1) -> true;
valid_guard(erlang, round, 1) -> true;
valid_guard(erlang, self, 0) -> true;
valid_guard(erlang, size, 1) -> true;
valid_guard(erlang, tl, 1) -> true;
valid_guard(erlang, trunc, 1) -> true;
valid_guard(erlang, tuple_size, 1) -> true;
valid_guard(erlang, is_binary, 1) -> true;
valid_guard(erlang, is_alive, 0) -> true;
valid_guard(erlang, is_boolean, 1) -> true;
valid_guard(erlang, is_function, 1) -> true;
valid_guard(erlang, is_function, 2) -> true;
valid_guard(erlang, is_integer, 1) -> true;
valid_guard(erlang, is_float, 1) -> true;
valid_guard(erlang, is_list, 1) -> true;
valid_guard(erlang, is_atom, 1) -> true;
valid_guard(erlang, is_number, 1) -> true;
valid_guard(erlang, is_pid, 1) -> true;
valid_guard(erlang, is_port, 1) -> true;
valid_guard(erlang, is_record, 2) -> true;
valid_guard(erlang, is_record, 3) -> true;
valid_guard(erlang, is_reference, 1) -> true;
valid_guard(erlang, is_tuple, 1) -> true;
valid_guard(erlang, 'and', 2) -> true;
valid_guard(erlang, 'or', 2) -> true;
valid_guard(erlang, '>', 2) -> true;
valid_guard(erlang, '<', 2) -> true;
valid_guard(erlang, '==', 2) -> true;
valid_guard(erlang, '=<', 2) -> true;
valid_guard(erlang, '>=', 2) -> true;
valid_guard(erlang, '/=', 2) -> true;
valid_guard(erlang, '=:=', 2) -> true;
valid_guard(erlang, '=/=', 2) -> true;
valid_guard(_, _, _) ->
    false.



