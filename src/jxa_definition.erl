%% -*- mode: Erlang; fill-column: 80; comment-column: 76; -*-
-module(jxa_definition).

-export([comp/3]).
-include_lib("joxa/include/joxa.hrl").

%%=============================================================================
%% Public API
%%=============================================================================
comp(Path0, Ctx0, ['defn+' | Details]) ->
    {Ctx2, Name, ArgList, Body} =
        compile_function(jxa_path:incr(Path0), Ctx0,
                         Details),
    Annots = jxa_annot:get_line(jxa_path:add_path(Path0),
                                jxa_ctx:annots(Ctx2)),
    jxa_ctx:add_exported_definition(Annots, Name, ArgList, Body, Ctx2);
comp(Path0, Ctx0, [defn | Details]) ->
    {Ctx1, Name, ArgList, Body} =
        compile_function(jxa_path:incr(Path0), Ctx0, Details),
    Annots = jxa_annot:get_line(jxa_path:add_path(Path0),
                                jxa_ctx:annots(Ctx1)),
    jxa_ctx:add_definition(Annots, Name, ArgList, Body, Ctx1);
comp(Path0, Ctx0, [definline | Details]) ->
    {Ctx1, Name, ArgList, Body} = compile_function(jxa_path:incr(Path0),
                                                   Ctx0, Details),
    Annots = jxa_annot:get_line(jxa_path:add_path(Path0),
                                inline,
                                jxa_ctx:annots(Ctx1)),
    jxa_ctx:add_definition(Annots, Name, ArgList, Body, Ctx1);
comp(Path0, Ctx0, Form = ['deftype+' | _]) ->
    jxa_spec:comp(Path0, Ctx0, Form);
comp(Path0, Ctx0, Form = [deftype | _]) ->
    jxa_spec:comp(Path0, Ctx0, Form);
comp(Path0, Ctx0, Form = [defspec | _]) ->
    jxa_spec:comp(Path0, Ctx0, Form);
comp(Path0, Ctx0, _) ->
    Idx = jxa_annot:get_idx(jxa_path:path(Path0),
                            jxa_ctx:annots(Ctx0)),
    ?JXA_THROW({invalid_definition, Idx}).


compile_function(Path0, Ctx0, Body = [Name, Args, _])
  when is_list(Args), is_atom(Name) ->
    compile_function1(Path0, Ctx0, [default_type() | Body]);
compile_function(Path0, Ctx0, Body = [_, Name, Args, _])
  when is_list(Args), is_atom(Name) ->
    compile_function1(jxa_path:incr(Path0), Ctx0, Body);
compile_function(Path0, Ctx0, _) ->
    Idx = jxa_annot:get_idx(jxa_path:path(Path0),
                            jxa_ctx:annots(Ctx0)),
    ?JXA_THROW({invalid_definition, Idx}).

compile_function1(Path0, Ctx0, [ReturnType, Name, Args, Expression])
  when is_atom(Name), is_list(Args) ->
    SpecArgs = lists:map(fun([SpecArgs, _Arg]) ->
                                 SpecArgs;
                            (_Arg) ->
                                 default_type()
                         end, Args),

    Ctx1 = jxa_spec:comp_implicit(Path0, Ctx0, Name, SpecArgs, ReturnType),
    {Ctx2, ArgList, Body} =
        jxa_expression:do_function_body(jxa_path:incr(Path0), Ctx1,
                                        Args, Expression),
    {Ctx2, Name, ArgList, Body}.


default_type() ->
    [{'__fun__', erlang, any}].
