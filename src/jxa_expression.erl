%% -*- mode: Erlang; fill-column: 80; comment-column: 76; -*-
-module(jxa_expression).

-export([do_function_body/4, comp/3, eval_args/3]).
-include_lib("joxa/include/joxa.hrl").

%%=============================================================================
%% Public API
%%=============================================================================
do_function_body(Path0, Ctx0, Args0, Expression) ->
    %% Funs can have types at any level. we just ignore them at
    %% Anything but the top
    Args1 = lists:map(fun([_, Arg]) ->
                              Arg;
                         (Arg) ->
                              Arg
                      end, Args0),
    {Ctx1, ArgList} = gen_args(jxa_path:add(Path0), Ctx0, Args1),

    Ctx2 = jxa_ctx:add_variables_to_scope(Args1, jxa_ctx:push_scope(Ctx1)),

    {Ctx3, Body} = comp(jxa_path:add(jxa_path:incr(Path0)),
                        Ctx2, Expression),
    {jxa_ctx:pop_scope(Ctx3), ArgList, Body}.

eval_args(Path0, Ctx0, Args0) ->
    {_, Ctx3, Args1} =
        lists:foldl(fun(Arg, {Path1, Ctx1, Acc}) ->
                            {Ctx2, Cerl} =
                                comp(jxa_path:add(Path1), Ctx1, Arg),
                            Path2 = jxa_path:incr(Path1),
                            {Path2, Ctx2, [Cerl | Acc]}
                    end, {Path0, Ctx0, []}, Args0),
    {Ctx3, lists:reverse(Args1)}.

comp(Path0, Ctx0, Arg) when is_binary(Arg) ->
    Annots = jxa_annot:get_line(jxa_path:path(Path0),
                                jxa_ctx:annots(Ctx0)),
    {Ctx0, cerl:ann_make_data(Annots, {atomic, Arg}, [])};
comp(Path0, Ctx0, Arg) when is_atom(Arg) ->

    case jxa_ctx:resolve_reference(Arg, -1, Ctx0) of
        {variable, Var} ->
            Annots  = jxa_annot:get_line(jxa_path:path(Path0),
                                         jxa_ctx:annots(Ctx0)),
            {Ctx0, cerl:set_ann(Var, Annots)};
        _ ->
            Idx  = jxa_annot:get_idx(jxa_path:path(Path0),
                                     jxa_ctx:annots(Ctx0)),
            ?JXA_THROW({undefined_reference, Arg, Idx})
    end;
comp(Path0, Ctx0, {'__fun__', F, A}) when is_integer(A) ->
    Annots = jxa_annot:get_line(jxa_path:path(Path0),
                                jxa_ctx:annots(Ctx0)),
    case jxa_ctx:resolve_reference(F, A, Ctx0) of
        {variable, Var} ->
            case cerl:is_c_fname(Var) andalso
                cerl:fname_arity(Var) == A of
                true ->
                    {Ctx0, cerl:set_ann(Var, Annots)};
                false ->
                    Idx = jxa_annot:get_idx(jxa_path:path(Path0),
                                            jxa_ctx:annots(Ctx0)),
                    ?JXA_THROW({invalid_reference, {F, A}, Idx})
            end;
        {apply, Name, A} ->
            {Ctx0, cerl:ann_c_fname(Annots, Name, A)};
        _ ->
            Idx = jxa_annot:get_idx(jxa_path:path(Path0),
                                    jxa_ctx:annots(Ctx0)),
            ?JXA_THROW({undefined_reference, {F, A}, Idx})
    end;
comp(Path0, Ctx0, Ref = {'__fun__', _, _, A}) when is_integer(A) ->
    Annots = jxa_annot:get_line(jxa_path:path(Path0),
                                jxa_ctx:annots(Ctx0)),
    case jxa_ctx:resolve_reference(Ref, A, Ctx0) of
        {variable, _Var} ->
            Idx = jxa_annot:get_idx(jxa_path:path(Path0),
                                    jxa_ctx:annots(Ctx0)),
            ?JXA_THROW({invalid_refrence, Ref, Idx});
        {remote, Module, Function} ->
            {Ctx0, cerl:ann_c_call(Annots,
                                   cerl:ann_c_atom(Annots,
                                                   erlang),
                                   cerl:ann_c_atom(Annots,
                                                   make_fun),
                                   [cerl:ann_c_atom(Annots, Module),
                                    cerl:ann_c_atom(Annots, Function),
                                    cerl:ann_c_int(Annots, A)])};
        _ ->
            Idx = jxa_annot:get_idx(jxa_path:path(Path0),
                                    jxa_ctx:annots(Ctx0)),
            ?JXA_THROW({undefined_reference, Ref, Idx})
    end;
comp(Path0, Ctx0, Arg) when is_integer(Arg) ->
    Annots = jxa_annot:get_line(jxa_path:path(Path0),
                                jxa_ctx:annots(Ctx0)),
    {Ctx0, cerl:ann_c_int(Annots, Arg)};
comp(Path0, Ctx0, Arg) when is_float(Arg) ->
    Annots = jxa_annot:get_line(jxa_path:path(Path0),
                                jxa_ctx:annots(Ctx0)),
    {Ctx0, cerl:ann_c_float(Annots, Arg)};
comp(Path0, Ctx0, Arg) when is_tuple(Arg) ->
    mk_tuple(Path0, Ctx0, tuple_to_list(Arg));
comp(Path0, Ctx0, Form = ['let' | _]) ->
    jxa_let:comp(Path0, Ctx0, Form);
comp(Path0, Ctx0, ['case', Expr | Clauses]) ->
    Annots = jxa_annot:get_line(jxa_path:path(Path0),
                                jxa_ctx:annots(Ctx0)),
    {Ctx1, CerlExpr} = comp(jxa_path:add(jxa_path:incr(Path0)), Ctx0, Expr),
    {Ctx2, CerlClauses} =
        jxa_clause:comp(jxa_path:incr(2, Path0), Ctx1, Clauses),
    {Ctx2, cerl:ann_c_case(Annots, CerlExpr, CerlClauses)};
comp(Path0, Ctx0, [do | Args]) ->
    mk_do(jxa_path:incr(Path0), Ctx0, Args);
comp(Path0, Ctx0, [values | Args0]) ->
    {Ctx2, Args1} = eval_args(jxa_path:incr(Path0), Ctx0, Args0),
    Annots = jxa_annot:get_line(jxa_path:path(Path0),
                                jxa_ctx:annots(Ctx0)),
    {Ctx2, cerl:ann_c_values(Annots, lists:reverse(Args1))};
comp(Path0, Ctx0, Expr = [binary | _]) ->
    jxa_binary:comp(Path0, Ctx0, Expr);
comp(Path0, Ctx0, [Arg1, '.', Arg2]) ->
    {Ctx1, Cerl1} = comp(Path0,
                         Ctx0, Arg1),
    {Ctx2, Cerl2} = comp(jxa_path:incr(2, Path0),
                         Ctx1, Arg2),
    Annots = jxa_annot:get_line(jxa_path:add_path(Path0),
                                jxa_ctx:annots(Ctx2)),
    {Ctx2, cerl:ann_c_cons(Annots, Cerl1, Cerl2)};
comp(Path0, Ctx0, [apply, {'__fun__', Module, Function, _A} | Args]) ->
    Annots = jxa_annot:get_line(jxa_path:add_path(Path0),
                                jxa_ctx:annots(Ctx0)),
    {Ctx1, ArgList} =  eval_args(jxa_path:incr(2, Path0),
                                 Ctx0, Args),
    {Ctx1, cerl:ann_c_call(Annots,
                           cerl:ann_c_atom(Annots,
                                           Module),
                           cerl:ann_c_atom(Annots,
                                           Function),
                           ArgList)};
comp(Path0, Ctx0, [apply, Target | Args]) ->
    Annots = jxa_annot:get_line(jxa_path:add_path(Path0),
                                jxa_ctx:annots(Ctx0)),
    {Ctx1, CerlTarget} = comp(jxa_path:add(jxa_path:incr(Path0)),
                              Ctx0, Target),
    {Ctx2, CerlArgList} =  eval_args(jxa_path:incr(2, Path0),
                                     Ctx1, Args),
    case cerl:is_c_fname(CerlTarget) of
        true ->
            case cerl:fname_arity(CerlTarget) == erlang:length(CerlArgList) of
                true ->
                    {Ctx2, cerl:ann_c_apply(Annots,
                                            CerlTarget,
                                            CerlArgList)};
                false ->
                    Idx = jxa_annot:get_line(jxa_path:add_path(Path0),
                                             jxa_ctx:annots(Ctx0)),
                    ?JXA_THROW({invalid_arity, Idx})
            end;
        false ->
            {Ctx2, cerl:ann_c_apply(Annots,
                                    CerlTarget,
                                    CerlArgList)}
    end;
comp(Path0, Ctx0, [cons, Arg1, Arg2]) ->
    {Ctx1, Cerl1} = comp(jxa_path:incr(Path0),
                         Ctx0, Arg1),
    {Ctx2, Cerl2} = comp(jxa_path:incr(2, Path0),
                         Ctx1, Arg2),
    Annots = jxa_annot:get_line(jxa_path:add_path(Path0),
                                jxa_ctx:annots(Ctx2)),
    {Ctx2, cerl:ann_c_cons(Annots, Cerl1, Cerl2)};
comp(Path0, Ctx0, [quote, Args]) ->
    Literal = jxa_literal:comp(jxa_path:add(jxa_path:incr(Path0)), Ctx0, Args),
    {Ctx0, Literal};
comp(Path0, Ctx0, [list | Args]) ->
    Path1 = jxa_path:incr(Path0),
    convert_list(Path1, Ctx0, Args);
comp(Path0, Ctx0, [fn | FnBody]) ->
    Annots = jxa_annot:get_line(jxa_path:add_path(Path0),
                                jxa_ctx:annots(Ctx0)),
    case FnBody of
        [Args, Expression] ->
            {Ctx1, ArgList, Body} =
                do_function_body(jxa_path:incr(Path0), Ctx0,
                                 Args, Expression),
            CerlFun = cerl:ann_c_fun(Annots, ArgList, Body),
            {Ctx1, CerlFun};
        _ ->
            Idx = jxa_annot:get_idx(jxa_path:add_path(Path0),
                                    jxa_ctx:annots(Ctx0)),
            ?JXA_THROW({invalid_fn_form, Idx})
    end;
comp(Path0, Ctx0, [tuple | Args]) ->
    mk_tuple(Path0, Ctx0, Args);
comp(Path0, Ctx0, Form = ['try' | _]) ->
    jxa_try:comp(Path0, Ctx0, Form);
comp(Path0, Ctx0, Form = [Val | Args]) ->
    Annots = jxa_annot:get_line(jxa_path:path(Path0), jxa_ctx:annots(Ctx0)),
    case jxa_annot:get_type(jxa_path:path(Path0), jxa_ctx:annots(Ctx0)) of
        string ->
            {Ctx0, cerl:ann_c_string(Annots, Form)};
        Type when Type == list; Type == vector ->
            PossibleArity = erlang:length(Args),
            Path1 = jxa_path:add(Path0),
            CallAnnots =
                jxa_annot:get_line(jxa_path:path(Path1),
                                   jxa_ctx:annots(Ctx0)),
            {Ctx1, ArgList} = eval_args(jxa_path:incr(Path0),
                                        Ctx0, Args),
            case jxa_ctx:resolve_reference(Val, PossibleArity, Ctx1) of
                {variable, Var} ->
                    {Ctx1, cerl:ann_c_apply(Annots,
                                            cerl:set_ann(Var, Annots),
                                            ArgList)};
                {apply, Name, Arity} ->
                    {Ctx1, cerl:ann_c_apply(Annots,
                                            cerl:ann_c_fname(CallAnnots,
                                                             Name,
                                                             Arity),
                                            ArgList)};
                {remote, Module, Function} ->
                    {Ctx1, cerl:ann_c_call(Annots,
                                           cerl:ann_c_atom(CallAnnots,
                                                           Module),
                                           cerl:ann_c_atom(CallAnnots,
                                                           Function),
                                           ArgList)};
                {error, Error1 = {mismatched_arity, _, _, _}} ->
                    Idx = jxa_annot:get_idx(jxa_path:path(Path0),
                                            jxa_ctx:annots(Ctx0)),
                    ?JXA_THROW({Error1, Idx});
                {error, Error2 = {mismatched_arity, _, _, _, _}} ->
                    Idx = jxa_annot:get_idx(jxa_path:path(Path0),
                                            jxa_ctx:annots(Ctx0)),
                    ?JXA_THROW({Error2, Idx});
                not_a_reference when is_list(Val) ->
                    {Ctx1, CerlVal} = comp(jxa_path:add(Path0), Ctx0, Val),
                    {Ctx1, cerl:ann_c_apply(Annots,
                                            CerlVal,
                                            ArgList)};
                not_a_reference ->
                    Idx = jxa_annot:get_idx(jxa_path:path(Path0),
                                            jxa_ctx:annots(Ctx0)),
                    ?JXA_THROW({invalid_reference, Val, PossibleArity, Idx})
            end
    end;
comp(Path0, Ctx0, Form) ->
    case jxa_annot:get_type(jxa_path:path(Path0), jxa_ctx:annots(Ctx0)) of
        string ->
            Annots = jxa_annot:get_line(jxa_path:path(Path0),
                                        jxa_ctx:annots(Ctx0)),
            {Ctx0, cerl:ann_c_string(Annots, Form)};
        _ ->
            Idx = jxa_annot:get_idx(jxa_path:path(Path0),
                                    jxa_ctx:annots(Ctx0)),
            ?JXA_THROW({invalid_form, Idx})
    end.

mk_tuple(Path0, Ctx0, Args) ->
    {_, Ctx3, Body} =
        lists:foldl(fun(Arg, {Path2, Ctx1, Acc}) ->
                            {Ctx2, Element} =
                                comp(jxa_path:add(Path2), Ctx1, Arg),
                            Path3 = jxa_path:incr(Path2),
                            {Path3, Ctx2, [Element | Acc]}
                    end, {Path0, Ctx0, []}, Args),
    Annots = jxa_annot:get_line(jxa_path:path(Path0),
                                jxa_ctx:annots(Ctx3)),
    {Ctx3, cerl:ann_c_tuple(Annots, lists:reverse(Body))}.

convert_list(_Path0, Ctx0, []) ->
    {Ctx0, cerl:c_nil()};
convert_list(Path0, Ctx0, [H | T]) ->
    {Ctx1, CerlH} = comp(jxa_path:add(Path0),
                         Ctx0, H),
    {Ctx2, CerlT} = convert_list(jxa_path:incr(Path0),
                                 Ctx1, T),
    Annots = jxa_annot:get_line(
               jxa_path:add_path(Path0),
               jxa_ctx:annots(Ctx2)),
    {Ctx2, cerl:ann_c_cons(Annots, CerlH, CerlT)}.

gen_args(Path0, Ctx0, Args0) ->
    {_, Ctx2, Args1} =
        lists:foldl(fun(Arg, {Path1, Ctx1, Acc})
                          when is_atom(Arg) ->
                            Annots =
                                jxa_annot:get_line(jxa_path:add_path(Path1),
                                                   jxa_ctx:annots(Ctx1)),
                            Path2 = jxa_path:incr(Path1),
                            {Path2, Ctx1, [cerl:ann_c_var(Annots, Arg) | Acc]};
                       (_Arg, {Path1, Ctx1, _}) ->
                            Path2 = jxa_path:incr(Path1),
                            Idx =
                                jxa_annot:get_idx(jxa_path:add_path(Path2),
                                                  jxa_ctx:annots(Ctx1)),
                            ?JXA_THROW({invalid_arg, Idx})
                    end, {Path0, Ctx0, []}, Args0),
    {Ctx2, lists:reverse(Args1)}.

mk_do(Path0, Ctx0, [Arg1, Arg2]) ->
    Annots =
        jxa_annot:get_line(jxa_path:add_path(Path0), jxa_ctx:annots(Ctx0)),
    {Ctx1, Cerl0} = comp(jxa_path:add(Path0), Ctx0, Arg1),
    {Ctx2, Cerl1} = comp(jxa_path:add(jxa_path:incr(Path0)), Ctx1, Arg2),
    {Ctx2, cerl:ann_c_seq(Annots, Cerl0, Cerl1)};
mk_do(Path0, Ctx0, [Arg1]) ->
    Annots =
        jxa_annot:get_line(jxa_path:add_path(Path0), jxa_ctx:annots(Ctx0)),
    {Ctx1, Cerl0} = comp(jxa_path:add(Path0), Ctx0, Arg1),
    {Ctx1, cerl:ann_c_seq(Annots, cerl:c_nil(), Cerl0)};
mk_do(Path0, Ctx0, [Arg1 | Rest]) ->
    Annots =
        jxa_annot:get_line(jxa_path:add_path(Path0), jxa_ctx:annots(Ctx0)),
    {Ctx1, Cerl0} = comp(jxa_path:add(Path0), Ctx0, Arg1),
    {Ctx2, Cerl1} = mk_do(jxa_path:incr(Path0), Ctx1, Rest),
    {Ctx2, cerl:ann_c_seq(Annots, Cerl0, Cerl1)}.


