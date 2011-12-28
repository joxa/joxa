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

comp_pattern(Path0, {Ctx0, Guards0}, Arg) when is_atom(Arg) ->
    {_, {Line, _}} = jxa_annot:get(jxa_path:path(Path0),
                                   jxa_ctx:annots(Ctx0)),
    case jxa_ctx:resolve_reference(Arg, -1, Ctx0) of
        {variable, Var} ->
            %% The reference already exists. So we create a new variable and
            %% add a guard for to test for equality
            GenSym = joxa:gensym(),
            CerlVar = cerl:ann_c_var([Line, compiler_generated], GenSym),
            Guards1 = [cerl:ann_c_call([Line, compiler_generated],
                                       cerl:ann_c_atom([Line,
                                                        compiler_generated],
                                                       'erlang'),
                                       cerl:ann_c_atom([Line,
                                                        compiler_generated],
                                                       '=:='),
                                       [CerlVar, cerl:set_ann(Var, [Line])])
                       | Guards0],
            %% We don't add the generated variable to the scope as we
            %% don't want it to be available to the user (The user really
            %% should not even be aware of it
            {{Ctx0, Guards1}, CerlVar};
        _ ->
            %% The variable is not in the scope so we turn it to a
            %% variable
            CerlVar = cerl:ann_c_var([Line], Arg),
            Ctx1 = jxa_ctx:add_variable_to_scope(Arg, Ctx0),
            {{Ctx1, Guards0}, CerlVar}
    end;
comp_pattern(Path0, Acc0={Ctx0, _}, Arg) when is_integer(Arg) ->
    {_, {Line, _}} = jxa_annot:get(jxa_path:path(Path0),
                                   jxa_ctx:annots(Ctx0)),
    {Acc0, cerl:ann_c_int([Line], Arg)};
comp_pattern(Path0, Acc0={Ctx0, _}, Arg) when is_float(Arg) ->
    {_, {Line, _}} = jxa_annot:get(jxa_path:path(Path0),
                                   jxa_ctx:annots(Ctx0)),
    {Acc0, cerl:ann_c_float([Line], Arg)};
comp_pattern(Path0, Acc0={Ctx0, _}, ['=', Arg1, Arg2])
  when is_atom(Arg1) ->
    {_, {Line, _}} = jxa_annot:get(jxa_path:path(Path0),
                                   jxa_ctx:annots(Ctx0)),
    {_, {Arg1Line, _}} = jxa_annot:get(jxa_path:path(jxa_path:incr(Path0)),
                                       jxa_ctx:annots(Ctx0)),
    {{Ctx1, Guards}, CerlPattern} =
        comp_pattern(jxa_path:incr(2, Path0),
                     Acc0, Arg2),
    CerlArg1 =
        cerl:ann_c_alias([Line], cerl:ann_c_var([Arg1Line], Arg1),
                         CerlPattern),
    {{jxa_ctx:add_variable_to_scope(Arg1, Ctx1), Guards}, CerlArg1};
comp_pattern(Path0, Acc0={Ctx0, _}, [quote, Args]) ->
    Literal = jxa_literal:comp(jxa_path:incr(Path0), Ctx0, Args),
    {Acc0, Literal};
comp_pattern(Path0, Acc0, Expr=[binary | _]) ->
    jxa_binary:comp_pattern(Path0, Acc0, Expr);
comp_pattern(Path0, Acc0, [Arg1, '.', Arg2]) ->
    {Acc1, Cerl1} = comp_pattern(Path0,
                         Acc0, Arg1),
    {Acc2={Ctx1, _, _}, Cerl2} = comp_pattern(jxa_path:incr(2, Path0),
                         Acc1, Arg2),
    {_, {Line, _}} = jxa_annot:get(jxa_path:add_path(Path0),
                                   jxa_ctx:annots(Ctx1)),
    {Acc2, cerl:ann_c_cons(Line, Cerl1, Cerl2)};
comp_pattern(Path0, Acc0, [cons, Arg1, Arg2]) ->
    {Acc1, Cerl1} = comp_pattern(jxa_path:incr(Path0),
                                 Acc0, Arg1),
    {Acc2={Ctx0, _, _}, Cerl2} = comp(jxa_path:incr(2, Path0),
                         Acc1, Arg2),
    {ident, {Line, _}} = jxa_annot:get(jxa_path:add_path(Path0),
                                       jxa_ctx:annots(Ctx0)),
    {Acc2, cerl:ann_c_cons(Line, Cerl1, Cerl2)};
comp_pattern(Path0, Acc0={Ctx0, _}, [quote, Args]) ->
    Literal = jxa_literal:comp(jxa_path:incr(Path0), Ctx0, Args),
    {Acc0, Literal};
comp_pattern(Path0, Acc0, [list | Args]) ->
    Path1 = jxa_path:incr(Path0),
    mk_list(Path1, Acc0, Args);
comp_pattern(Path0, Acc0, [tuple | Args]) ->
    mk_tuple(Path0, Acc0, Args);
comp_pattern(Path0, Ctx0, Arg) when is_tuple(Arg) ->
    mk_tuple(Path0, Ctx0, tuple_to_list(Arg));
comp_pattern(Path0, Acc0={Ctx0, _}, ['=', Arg1, Arg2])
  when is_atom(Arg2) ->
    {_, {Line, _}} = jxa_annot:get(jxa_path:path(Path0),
                                   jxa_ctx:annots(Ctx0)),
    {_, {Arg2Line, _}} = jxa_annot:get(jxa_path:path(jxa_path:incr(Path0)),
                                       jxa_ctx:annots(Ctx0)),
    {Acc1, CerlPattern} =
        comp_pattern(jxa_path:incr(Path0),
                     Acc0, Arg1),
    CerlArg1 =
        cerl:ann_c_alias([Line], cerl:ann_c_var([Arg2Line], Arg2),
                         CerlPattern),
    {Acc1, CerlArg1}.

mk_guards(GuardLine, []) ->
    cerl:ann_c_atom([GuardLine, compiler_generated], true);
mk_guards(_GuardLine, [Guard]) ->
    Guard;
mk_guards(GuardLine, [Pattern | Rest]) ->
    cerl:ann_c_call([GuardLine, compiler_generated],
                    cerl:ann_c_atom([GuardLine, compiler_generated],
                                    'erlang'),
                    cerl:ann_c_atom([GuardLine, compiler_generated],
                                    'and'),
                    [Pattern, mk_guards(GuardLine, Rest)]).

%%=============================================================================
%% Private API
%%=============================================================================
comp(Path0, Ctx0, [], _Acc) ->
    {_, {Line, _}} = jxa_annot:get(jxa_path:path(Path0),
                                   jxa_ctx:annots(Ctx0)),
    ?JXA_THROW({no_clauses_provided, Line});
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
    {_, {Line, _}} = jxa_annot:get(jxa_path:path(Path0),
                                   jxa_ctx:annots(Ctx1)),
    Var = joxa:gensym(),
    Annots = [Line, compiler_generated],
    {Ctx1,
     ActualClause,
     [cerl:ann_c_clause(Annots,
                        [cerl:ann_c_var(Annots, Var)],
                        cerl:ann_c_primop(Annots,
                                          cerl:ann_c_atom(Annots, 'match_fail'),
                                          [cerl:ann_c_tuple(Annots,
                                                            [cerl:ann_c_atom(Annots, case_clause),
                                                             cerl:ann_c_var(Annots, Var)])]))]}.



compile_clause_body(Path0, Ctx0, [Pattern, Guards, Body]) ->
    Ctx1 = jxa_ctx:push_scope(Ctx0),
    {_, {Line, _}} = jxa_annot:get(jxa_path:path(Path0),
                                   jxa_ctx:annots(Ctx1)),
    {{Ctx2, PatternGuards}, CerlPattern} =
        comp_pattern(jxa_path:add(Path0), {Ctx1, []}, Pattern),
    {_, {GuardLine, _}} =
        jxa_annot:get(jxa_path:add_path(jxa_path:incr(Path0)),
                      jxa_ctx:annots(Ctx2)),
    {Ctx3, CerlGuard} =
        jxa_expression:comp(jxa_path:add(jxa_path:incr(Path0)),
                            Ctx2, Guards),
    CompleteGuards = mk_guards(GuardLine,
                               [CerlGuard | PatternGuards]),

    {Ctx4, CerlBody} =
        jxa_expression:comp(jxa_path:add(jxa_path:incr(2, Path0)),
                            Ctx3, Body),
    {jxa_ctx:pop_scope(Ctx4),
     cerl:ann_c_clause([Line], [CerlPattern], CompleteGuards,
                       CerlBody)};
compile_clause_body(Path0, Ctx0, [Pattern, Body]) ->
    Ctx1 = jxa_ctx:push_scope(Ctx0),
    {_, {Line, _}} = jxa_annot:get(jxa_path:path(Path0),
                                   jxa_ctx:annots(Ctx1)),
    {{Ctx2, PatternGuards}, CerlPattern} =
        comp_pattern(jxa_path:add(Path0), {Ctx1, []}, Pattern),
    {Ctx3, CerlBody} =
        jxa_expression:comp(jxa_path:add(jxa_path:incr(Path0)),
                                           Ctx2, Body),
    CompleteGuards = mk_guards(Line, PatternGuards),

    {jxa_ctx:pop_scope(Ctx3),
     cerl:ann_c_clause([Line], [CerlPattern],
                       CompleteGuards,
                       CerlBody)}.

mk_tuple(Path0, Acc0, Args) ->
    {_, Acc3={Ctx0, _}, Body} =
        lists:foldl(fun(Arg, {Path2, Acc1, LAcc}) ->
                            {Acc2, Element} =
                                comp_pattern(jxa_path:add(Path2), Acc1, Arg),
                            Path3 = jxa_path:incr(Path2),
                            {Path3, Acc2, [Element | LAcc]}
                    end, {Path0, Acc0, []}, Args),
    {_, {Line, _}} = jxa_annot:get(jxa_path:path(Path0),
                                   jxa_ctx:annots(Ctx0)),
    {Acc3, cerl:ann_c_tuple([Line], lists:reverse(Body))}.

mk_list(_Path0, Acc0, []) ->
    {Acc0, cerl:c_nil()};
mk_list(Path0, Acc0, [H | T]) ->
    {Acc1, CerlH} = comp_pattern(jxa_path:add(Path0),
                                 Acc0, H),
    {Acc2={Ctx0, _}, CerlT} = mk_list(jxa_path:incr(Path0),
                            Acc1, T),
    {_, {Line, _}} = jxa_annot:get(
                       jxa_path:add_path(Path0),
                       jxa_ctx:annots(Ctx0)),
    {Acc2, cerl:ann_c_cons([Line], CerlH, CerlT)}.
