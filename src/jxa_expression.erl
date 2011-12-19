%% -*- mode: Erlang; fill-column: 80; comment-column: 76; -*-
-module(jxa_expression).

-export([do_function_body/4, comp/3]).
-include_lib("joxa/include/joxa.hrl").

%%=============================================================================
%% Public API
%%=============================================================================
do_function_body(Path0, Ctx0, Args, Expression) ->
    {Ctx1, ArgList} = gen_args(Path0, Ctx0, Args),
    Ctx2 = jxa_ctx:add_variables_to_scope(Args, jxa_ctx:push_scope(Ctx1)),

    {Ctx3, Body} = comp(jxa_path:add(jxa_path:incr(2, Path0)),
                        Ctx2, Expression),
    {jxa_ctx:pop_scope(Ctx3), ArgList, Body}.

comp(Path0, Ctx0, Arg) when is_atom(Arg) ->
    {_, Idx = {Line, _}} = jxa_annot:get(jxa_path:path(Path0),
                                         jxa_ctx:annots(Ctx0)),
    case jxa_ctx:resolve_reference(Arg, -1, Ctx0) of
        variable ->
            {Ctx0, cerl:ann_c_var([Line], Arg)};
        _ ->
            ?JXA_THROW({undefined_reference, Idx})
    end;

comp(Path0, Ctx0, Arg) when is_atom(Arg) ->
    {_, {Line, _}} = jxa_annot:get(jxa_path:path(Path0),
                                   jxa_ctx:annots(Ctx0)),
    {Ctx0, cerl:ann_c_atom([Line], Arg)};
comp(Path0, Ctx0, Arg) when is_integer(Arg) ->
    {_, {Line, _}} = jxa_annot:get(jxa_path:path(Path0),
                                   jxa_ctx:annots(Ctx0)),
    {Ctx0, cerl:ann_c_int([Line], Arg)};
comp(Path0, Ctx0, Arg) when is_float(Arg) ->
    {_, {Line, _}} = jxa_annot:get(jxa_path:path(Path0),
                                   jxa_ctx:annots(Ctx0)),
    {Ctx0, cerl:ann_c_float([Line], Arg)};
comp(Path0, Ctx0, Form = ['let' | _]) ->
    jxa_let:comp(Path0, Ctx0, Form);
comp(Path0, Ctx0, [do | Args]) ->
    mk_do(jxa_path:incr(Path0), Ctx0, Args);
comp(Path0, Ctx0, [values | Args0]) ->
    {Ctx2, Args1} = eval_args(jxa_path:incr(Path0), Ctx0, Args0),
    {_, {Line, _}} = jxa_annot:get(jxa_path:path(Path0),
                                   jxa_ctx:annots(Ctx0)),
    {Ctx2, cerl:ann_c_values([Line], lists:reverse(Args1))};
comp(Path0, Ctx0, [Arg1, '.', Arg2]) ->
    {Ctx1, Cerl1} = comp(Path0,
                         Ctx0, Arg1),
    {Ctx2, Cerl2} = comp(jxa_path:incr(2, Path0),
                         Ctx1, Arg2),
    {_, {Line, _}} = jxa_annot:get(jxa_path:add_path(Path0),
                                   jxa_ctx:annots(Ctx2)),
    {Ctx2, cerl:ann_c_cons(Line, Cerl1, Cerl2)};
comp(Path0, Ctx0, [cons, Arg1, Arg2]) ->
    {Ctx1, Cerl1} = comp(jxa_path:incr(Path0),
                         Ctx0, Arg1),
    {Ctx2, Cerl2} = comp(jxa_path:incr(2, Path0),
                         Ctx1, Arg2),
    {ident, {Line, _}} = jxa_annot:get(jxa_path:add_path(Path0),
                                       jxa_ctx:annots(Ctx2)),
    {Ctx2, cerl:ann_c_cons(Line, Cerl1, Cerl2)};
comp(Path0, Ctx0, [quote, Args]) ->
    Path1 = jxa_path:add(Path0),
    Literal = jxa_literal:comp(Path1, Ctx0, Args),
    {Ctx0, Literal};
comp(Path0, Ctx0, [list | Args]) ->
    Path1 = jxa_path:incr(Path0),
    convert_list(Path1, Ctx0, Args);
comp(Path0, Ctx0, [vector | Args]) ->
    convert_vector(Path0, Ctx0, Args);
comp(Path0, Ctx0, Form = [Val | Args]) ->
    case jxa_annot:get(jxa_path:path(Path0), jxa_ctx:annots(Ctx0)) of
        {string, {Line, _}} ->
            {Ctx0, cerl:ann_c_string([Line], Form)};
        {vector, {_, _}} ->
            convert_vector(Path0, Ctx0, Form);
        {Type, Idx={BaseLine, _}} when Type == list; Type == vector ->
            PossibleArity = erlang:length(Args),
            Path1 = jxa_path:add(Path0),
            {_, {CallLine, _}} =
                jxa_annot:get(jxa_path:path(Path1),
                              jxa_ctx:annots(Ctx0)),
            {Ctx1, ArgList} = eval_args(jxa_path:incr(Path0),
                                        Ctx0, Args),
            case jxa_ctx:resolve_reference(Val, PossibleArity, Ctx0) of
                variable ->
                    {Ctx1, cerl:ann_c_apply([BaseLine],
                                            cerl:ann_c_var([CallLine],
                                                           Val,
                                                           PossibleArity),
                                            ArgList)};
                {apply, Name, Arity} ->
                    {Ctx1, cerl:ann_c_apply([BaseLine],
                                            cerl:ann_c_fname([CallLine],
                                                             Name,
                                                             Arity),
                                            ArgList)};
                {remote, Module, Function} ->
                    {Ctx1, cerl:ann_c_call([BaseLine],
                                           cerl:ann_c_atom([CallLine],
                                                           Module),
                                           cerl:ann_c_atom([CallLine],
                                                           Function),
                                           ArgList)};
                {error, Error1 = {mismatched_arity, _, _, _}} ->
                    ?JXA_THROW({Error1, Idx});
                {error, Error2 = {mismatched_arity, _, _, _, _}} ->
                    ?JXA_THROW({Error2, Idx});
                invalid ->
                    %% The last thing it might be is a function call. So we
                    %% are going to try to compile it. It might work
                    {Ctx1, Cerl} = comp(Path1, Ctx1, Val),
                    {Ctx1, cerl:ann_c_apply([BaseLine], Cerl, ArgList)}
            end
    end;
comp(Path0, Ctx0, _Form) ->
    {_, Idx} = jxa_annot:get(jxa_path:path(Path0), jxa_ctx:annots(Ctx0)),
    ?JXA_THROW({invalid_form, Idx}).

convert_vector(Path0, Ctx0, Args) ->
    {_, Ctx3, Body} =
        lists:foldl(fun(Arg, {Path2, Ctx1, Acc}) ->
                            {Ctx2, Element} =
                                comp(jxa_path:add(Path2), Ctx1, Arg),
                            Path3 = jxa_path:incr(Path2),
                            {Path3, Ctx2, [Element | Acc]}
                    end, {Path0, Ctx0, []}, Args),
    {_, {Line, _}} = jxa_annot:get(jxa_path:path(Path0),
                                   jxa_ctx:annots(Ctx3)),
    {Ctx3, cerl:ann_c_tuple([Line], lists:reverse(Body))}.

convert_list(_Path0, Ctx0, []) ->
    {Ctx0, cerl:c_nil()};
convert_list(Path0, Ctx0, [H | T]) ->
    {Ctx1, CerlH} = comp(jxa_path:add(Path0),
                         Ctx0, H),
    {Ctx2, CerlT} = convert_list(jxa_path:incr(Path0),
                                 Ctx1, T),
    {_, {Line, _}} = jxa_annot:get(
                       jxa_path:add_path(Path0),
                       jxa_ctx:annots(Ctx2)),
    {Ctx2, cerl:ann_c_cons([Line], CerlH, CerlT)}.

eval_args(Path0, Ctx0, Args0) ->
    {_, Ctx3, Args1} =
        lists:foldl(fun(Arg, {Path1, Ctx1, Acc}) ->
                            {Ctx2, Cerl} =
                                comp(jxa_path:add(Path1), Ctx1, Arg),
                            Path2 = jxa_path:incr(Path1),
                            {Path2, Ctx2, [Cerl | Acc]}
                    end, {Path0, Ctx0, []}, Args0),
    {Ctx3, lists:reverse(Args1)}.

gen_args(Path0, Ctx0, Args0) ->
    {_, Ctx2, Args1} =
        lists:foldl(fun(Arg, {Path1, Ctx1, Acc})
                          when is_atom(Arg) ->
                            Path2 = jxa_path:incr(Path1),
                            {_, {Line, _}} =
                                jxa_annot:get(jxa_path:add_path(Path2),
                                              jxa_ctx:annots(Ctx1)),
                            {Path2, Ctx1, [cerl:ann_c_var([Line], Arg) | Acc]};
                       (_Arg, {Path1, Ctx1, _}) ->
                            Path2 = jxa_path:incr(Path1),
                            {_, {Line, Char}} =
                                jxa_annot:get(jxa_path:add_path(Path2),
                                              jxa_ctx:annots(Ctx1)),
                            ?JXA_THROW({invalid_arg, Line, Char})
                    end, {Path0, Ctx0, []}, Args0),
    {Ctx2, lists:reverse(Args1)}.

mk_do(Path0, Ctx0, [Arg1, Arg2]) ->
    {_, {Line, _}} =
        jxa_annot:get(jxa_path:add_path(Path0), jxa_ctx:annots(Ctx0)),
    {Ctx1, Cerl0} = comp(jxa_path:add(Path0), Ctx0, Arg1),
    {Ctx2, Cerl1} = comp(jxa_path:add(jxa_path:incr(Path0)), Ctx1, Arg2),
    {Ctx2, cerl:ann_c_seq([Line], Cerl0, Cerl1)};
mk_do(Path0, Ctx0, [Arg1]) ->
    {_, {Line, _}} =
        jxa_annot:get(jxa_path:add_path(Path0), jxa_ctx:annots(Ctx0)),
    {Ctx1, Cerl0} = comp(jxa_path:add(Path0), Ctx0, Arg1),
    {Ctx1, cerl:ann_c_seq([Line], cerl:c_nil(), Cerl0)};
mk_do(Path0, Ctx0, [Arg1 | Rest]) ->
    {_, {Line, _}} =
        jxa_annot:get(jxa_path:add_path(Path0), jxa_ctx:annots(Ctx0)),
    {Ctx1, Cerl0} = comp(jxa_path:add(Path0), Ctx0, Arg1),
    {Ctx2, Cerl1} = mk_do(jxa_path:incr(Path0), Ctx1, Rest),
    {Ctx2, cerl:ann_c_seq([Line], Cerl0, Cerl1)}.

