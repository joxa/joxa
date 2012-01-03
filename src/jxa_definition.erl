%% -*- mode: Erlang; fill-column: 80; comment-column: 76; -*-
-module(jxa_definition).

-export([comp/3]).
-include_lib("joxa/include/joxa.hrl").

%%=============================================================================
%% Public API
%%=============================================================================
comp(Path0, Ctx0, ['defn+', Name, Args, Expression])
  when is_atom(Name), is_list(Args) ->
    Ctx1 = jxa_ctx:add_def_placeholder(Name, erlang:length(Args), Ctx0),
    {Ctx2, ArgList, Body} =
        jxa_expression:do_function_body(jxa_path:incr(2, Path0), Ctx1,
                                        Args, Expression),
    Annots = jxa_annot:get_line(jxa_path:add_path(Path0),
                                jxa_ctx:annots(Ctx2)),
    jxa_ctx:add_exported_definition(Annots, Name, ArgList, Body, Ctx2);
comp(Path0, Ctx0, [defn, Name, Args, Expression]) ->
    Ctx1 = jxa_ctx:add_def_placeholder(Name, erlang:length(Args), Ctx0),
    {Ctx2, ArgList, Body} =
        jxa_expression:do_function_body(jxa_path:incr(2, Path0), Ctx1,
                                        Args, Expression),
    Annots = jxa_annot:get_line(jxa_path:add_path(Path0),
                                jxa_ctx:annots(Ctx2)),
    jxa_ctx:add_definition(Annots, Name, ArgList, Body, Ctx1);
comp(Path0, Ctx0, [definline, Name, Args, Expression]) ->
    Ctx1 = jxa_ctx:add_def_placeholder(Name, erlang:length(Args), Ctx0),
    {Ctx2, ArgList, Body} =
        jxa_expression:do_function_body(jxa_path:incr(2, Path0), Ctx1,
                                        Args, Expression),
    Annots = jxa_annot:get_line(jxa_path:add_path(Path0),
                                inline,
                                jxa_ctx:annots(Ctx2)),
    jxa_ctx:add_definition(Annots, Name, ArgList, Body, Ctx1);
comp(Path0, Ctx0, _) ->
    Idx = jxa_annot:get_idx(jxa_path:path(Path0),
                            jxa_ctx:annots(Ctx0)),
    ?JXA_THROW({invalid_definition, Idx}).






