%% -*- mode: Erlang; fill-column: 80; comment-column: 76; -*-
-module(jxa_spec).

-export([comp/3, comp_implicit/5]).
-include_lib("joxa/include/joxa.hrl").

%%=============================================================================
%% Public API
%%=============================================================================
comp_implicit(Path0, Ctx0,  Name, Args, Expression) ->
    Annots = jxa_annot:get_line(jxa_path:path(Path0),
                                jxa_ctx:annots(Ctx0)),
    Arity = erlang:length(Args),

    {Ctx1, Body} = def_anon_function(jxa_path:incr(2, Path0),
                                     Ctx0, Args, Expression),

    jxa_ctx:add_type(Name, Arity,
                     {{c_literal,Annots,spec},
                      {c_literal,
                       Annots,
                       [{{Name,Arity},
                         [Body]}]}}, Ctx1).

comp(Path0, Ctx0, ['deftype+', Name, Args, Expression])
  when is_atom(Name), is_list(Args) ->
    Arity = erlang:length(Args),
    Ctx1 = jxa_ctx:add_type_export(Name, Arity, Ctx0),
    def_top_level_function(jxa_path:incr(Path0), Ctx1,
                           Name, Args, Expression);
comp(Path0, Ctx0, [deftype, Name, Args, Expression]) ->
    def_top_level_function(jxa_path:incr(Path0), Ctx0,
                           Name, Args, Expression);
comp(Path0, Ctx0, [defspec, Name, Args, Expression]) ->
    comp_implicit(jxa_path:incr(Path0), Ctx0, Name, Args, Expression);
comp(Path0, Ctx0, _) ->
    Idx = jxa_annot:get_idx(jxa_path:path(Path0),
                            jxa_ctx:annots(Ctx0)),
    ?JXA_THROW({invalid_type_definition, Idx}).

%%========================= ====================================================
%% Internal API
%%=============================================================================
def_top_level_function(Path0, Ctx0, Name, Args, Expression) ->
    Idx = {Line, _} = jxa_annot:get_idx(jxa_path:path(Path0),
                                        jxa_ctx:annots(Ctx0)),
    Arity = erlang:length(Args),
    Ctx1 = jxa_ctx:push_type_scope(Ctx0),

    Ctx2 =
        jxa_ctx:add_type_variable_to_scope(Name,
                                           Arity,
                                           Ctx1),
    Ctx4 = lists:foldl(fun(Arg, Ctx3) when is_atom(Arg) ->
                               jxa_ctx:add_type_variable_to_scope(Arg, -1, Ctx3);
                          (_Arg, _) ->
                               ?JXA_THROW({invalid_type_definition, Idx})
                       end, Ctx2, Args),
    {Ctx5, Expr} = comp_expr(jxa_path:incr(3, Path0),
                             Ctx4, Expression),

    jxa_ctx:add_type(Name, Arity,
                     {{c_literal,[Line],type},
                      {c_literal,[Line],
                       [{Name, Expr, lists:map(fun(El) ->
                                                       {var, Line, El}
                                               end, Args)}]}},
                     jxa_ctx:pop_type_scope(Ctx5)).

def_anon_function(Path0, Ctx0, Args0, Expression) ->
    {Line, _} = jxa_annot:get_idx(jxa_path:path(Path0),
                                  jxa_ctx:annots(Ctx0)),
    {_, Ctx3, Args1} =
        lists:foldl(fun(Arg, {Path1, Ctx1, Acc}) ->
                            {Ctx2, ArgExpr} = comp_expr(Path1, Ctx1, Arg),
                            {jxa_path:incr(Path1), Ctx2, [ArgExpr | Acc]}
                    end, {Path0, Ctx0, []}, Args0),
    {Ctx5, Expr} = comp_expr(jxa_path:incr(2, Path0),
                             Ctx3, Expression),

    {Ctx5, [{type,Line,'fun',
             [{type,Line,product, lists:reverse(Args1)},
              Expr]}]}.



comp_expr(Path0, Ctx0, [quote, Value]) ->
    {Ctx0, mk_literal(jxa_path:add(jxa_path:incr(Path0)), Ctx0, Value)};
comp_expr(Path0, Ctx0, Arg) when is_tuple(Arg) ->
    {Line, _} = jxa_annot:get_idx(jxa_path:path(Path0),
                                  jxa_ctx:annots(Ctx0)),
    {_, Ctx3, TupleValues} =
        lists:foldl(fun(El, {Path1, Ctx1, Acc}) ->
                            {Ctx2, CompEl} =
                                comp_expr(Path1,
                                          Ctx1, El),
                            {jxa_path:incr(Path1), Ctx2, [CompEl | Acc]}
                    end, {Path0, Ctx0, []}, tuple_to_list(Arg)),
    {Ctx3, {tuple, Line, list_to_tuple(lists:reverse(TupleValues))}};
comp_expr(Path0, Ctx0, Arg) when is_integer(Arg) ->
    {Ctx0, mk_literal(Path0, Ctx0, Arg)};
comp_expr(Path0, Ctx0, [binary]) ->
    {Line, _} = jxa_annot:get_idx(jxa_path:path(Path0),
                                  jxa_ctx:annots(Ctx0)),
    {Ctx0, {type,Line,binary, [{integer,Line,0},{integer,Line,0}]}};
comp_expr(Path0, Ctx0, [binary, Arg])  when is_integer(Arg) ->
    {Line, _} = jxa_annot:get_idx(jxa_path:path(Path0),
                                  jxa_ctx:annots(Ctx0)),
    {Ctx0, {type,Line,binary, [{integer,Line,Arg},{integer,Line,0}]}};
comp_expr(Path0, Ctx0, [binary, '*', Arg])  when is_integer(Arg) ->
    {Line, _} = jxa_annot:get_idx(jxa_path:path(Path0),
                                  jxa_ctx:annots(Ctx0)),
    {Ctx0, {type,Line,binary, [{integer,Line,0},{integer,Line,Arg}]}};
comp_expr(Path0, Ctx0, [binary, Arg1, '*', Arg2])
  when is_integer(Arg1), is_integer(Arg2) ->
    {Line, _} = jxa_annot:get_idx(jxa_path:path(Path0),
                                  jxa_ctx:annots(Ctx0)),
    {Ctx0, {type,Line,binary, [{integer,Line,Arg1},{integer,Line,Arg2}]}};
comp_expr(Path0, Ctx0, [binary | _]) ->
    Idx = jxa_annot:get_idx(jxa_path:path(Path0),
                            jxa_ctx:annots(Ctx0)),
    ?JXA_THROW({invalid_binary_type_spec, Idx});
comp_expr(Path0, Ctx0, ['fn']) ->
    {Line, _} = jxa_annot:get_idx(jxa_path:path(Path0),
                                  jxa_ctx:annots(Ctx0)),
    {Ctx0, {type, Line, 'fun', []}};
comp_expr(Path0, Ctx0, ['fn', ['...'], Expr]) ->
    {Line, _} = jxa_annot:get_idx(jxa_path:path(Path0),
                                  jxa_ctx:annots(Ctx0)),
    {Ctx1, CompExpr} = comp_expr(jxa_path:add(jxa_path:incr(2, Path0)),
                                 Ctx0, Expr),
    {Ctx1, {type, Line, 'fun', [{type, Line, any}, CompExpr]}};
comp_expr(Path0, Ctx0, ['fn', Args, Expr]) ->
    def_anon_function(jxa_path:incr(Path0),
                      Ctx0, Args, Expr);
comp_expr(Path0, Ctx0, [list, Arg]) ->
    {Line, _} = jxa_annot:get_idx(jxa_path:path(Path0),
                                  jxa_ctx:annots(Ctx0)),
    {Ctx1, List} = comp_expr(jxa_path:add(jxa_path:incr(Path0)),
                             Ctx0, Arg),
    {Ctx1, {list, Line, List}};
comp_expr(Path0, Ctx0, Name) when is_atom(Name) ->
    Idx = {Line, _} = jxa_annot:get_idx(jxa_path:path(Path0),
                                        jxa_ctx:annots(Ctx0)),
    case jxa_ctx:resolve_type_variable(Name, -1, Ctx0) of
        true ->
            {Ctx0, {var, Line, Name}};
        false ->
            ?JXA_THROW({invalid_type_reference, Idx})
    end;
comp_expr(Path0, Ctx0, [{'__fun__', erlang, range}, A1, A2])
  when is_integer(A1), is_integer(A2) ->
    %% Things in erlang get treated a bit differently then things in
    %% other modules. It sucks that they have to be special
    {Line, _} = jxa_annot:get_idx(jxa_path:path(Path0),
                                  jxa_ctx:annots(Ctx0)),
    {_, Ctx3, CompArgs} =
        lists:foldl(fun(El, {Path1, Ctx1, Acc}) ->
                            {Ctx2, CompEl} =
                                comp_expr(Path1, Ctx1, El),
                            {jxa_path:incr(Path1),
                             Ctx2, [CompEl | Acc]}
                    end, {jxa_path:incr(Path0), Ctx0, []},
                    [A1, A2]),
    {Ctx3, {type, Line, range,
            lists:reverse(CompArgs)}};
comp_expr(Path0, Ctx0, [{'__fun__', erlang, Func} | Args]) ->
    %% Things in erlang get treated a bit differently then things in
    %% other modules. It sucks that they have to be special
    {Line, _} = jxa_annot:get_idx(jxa_path:path(Path0),
                                  jxa_ctx:annots(Ctx0)),
    {_, Ctx3, CompArgs} =
        lists:foldl(fun(El, {Path1, Ctx1, Acc}) ->
                            {Ctx2, CompEl} =
                                comp_expr(Path1, Ctx1, El),
                            {jxa_path:incr(Path1),
                             Ctx2, [CompEl | Acc]}
                    end, {jxa_path:incr(Path0), Ctx0, []},
                    Args),
    {Ctx3, {type, Line, Func,
            lists:reverse(CompArgs)}};
comp_expr(Path0, Ctx0, [{'__fun__', Module, Func} | Args])
  when is_atom(Module), is_atom(Func) ->
    {Line, _} = jxa_annot:get_idx(jxa_path:path(Path0),
                                  jxa_ctx:annots(Ctx0)),
    {_, Ctx3, CompArgs} =
        lists:foldl(fun(El, {Path1, Ctx1, Acc}) ->
                            {Ctx2, CompEl} =
                                comp_expr(Path1, Ctx1, El),
                            {jxa_path:incr(Path1),
                             Ctx2, [CompEl | Acc]}
                    end, {jxa_path:incr(Path0), Ctx0, []},
                    Args),
    {Ctx3, {remote_type, Line, [{atom, Line, Module},
                                {atom, Line, Func},
                                lists:reverse(CompArgs)]}};
comp_expr(Path0, Ctx0, [Var | Args]) when is_atom(Var) ->
    ArgCount = erlang:length(Args),
    Idx = {Line, _} = jxa_annot:get_idx(jxa_path:path(Path0),
                                        jxa_ctx:annots(Ctx0)),
    case jxa_ctx:resolve_type_variable(Var, ArgCount, Ctx0) of
        false ->
            ?JXA_THROW({invalid_type_reference, Idx});
        true ->
            {_, Ctx3, CompArgs} =
                lists:foldl(fun(El, {Path1, Ctx1, Acc}) ->
                                    {Ctx2, CompEl} =
                                        comp_expr(Path1, Ctx1, El),
                                    {jxa_path:incr(Path1),
                                     Ctx2, [CompEl | Acc]}
                            end, {jxa_path:incr(Path0), Ctx0, []},
                            Args),
            {Ctx3, {type, Line, Var, CompArgs}}
    end.

mk_literal(Path0, Ctx0, Arg) when is_atom(Arg) ->
    {Line, _} = jxa_annot:get_idx(jxa_path:path(Path0),
                                  jxa_ctx:annots(Ctx0)),
    {atom, Line, Arg};
mk_literal(Path0, Ctx0, Arg) when is_integer(Arg) ->
    {Line, _} = jxa_annot:get_idx(jxa_path:path(Path0),
                                  jxa_ctx:annots(Ctx0)),
    {integer, Line, Arg};
mk_literal(Path0, Ctx0, [Args]) ->
    {Line, _} = jxa_annot:get_idx(jxa_path:path(Path0),
                                  jxa_ctx:annots(Ctx0)),

    {list, Line, mk_literal(jxa_path:add(Path0), Ctx0, Args)};
mk_literal(Path0, Ctx0, Args) when is_tuple(Args) ->
    {Line, _} = jxa_annot:get_idx(jxa_path:path(Path0),
                                  jxa_ctx:annots(Ctx0)),
    Literals = lists:foldl(fun(El, {Path1, Acc}) ->
                                   {jxa_path:incr(Path1),
                                    [mk_literal(Path1, Ctx0, El) |
                                     Acc]}
                           end, {Path0, []}, tuple_to_list(Args)),
    {list, Line, lists:reverse(Literals)};
mk_literal(Path0, Ctx0, Args) when is_tuple(Args) ->
    {Line, _} = jxa_annot:get_idx(jxa_path:path(Path0),
                                  jxa_ctx:annots(Ctx0)),
    Literals = lists:foldl(fun(El, {Path1, Acc}) ->
                                   {jxa_path:incr(Path1),
                                    [mk_literal(Path1, Ctx0, El) |
                                     Acc]}
                           end, {Path0, []}, tuple_to_list(Args)),
    {tuple, Line, lists:reverse(Literals)};
mk_literal(Path0, Ctx0, _) ->
    Idx = jxa_annot:get_idx(jxa_path:path(Path0),
                            jxa_ctx:annots(Ctx0)),
    ?JXA_THROW({invalid_type_literal, Idx}).











