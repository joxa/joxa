%% -*- mode: Erlang; fill-column: 76; comment-column: 76; -*-
-module(joxa).

-export([comp/1, comp/2, format_exception/1]).

-include_lib("joxa/include/joxa.hrl").
%%=============================================================================
%% Types
%%=============================================================================

%%=============================================================================
%% Public API
%%=============================================================================
-spec comp(string() | binary()) -> {jxa_ctx:ctx(), binary()}.
comp(FileName) ->
   case file:read_file(FileName) of
       {ok, Binary} ->
           ModuleName = extract_module_name(FileName),
           Result = {_, Binary} = comp(ModuleName, Binary),
           {module, ModuleName} =
               code:load_binary(ModuleName, FileName, Binary),
           Result;
       {error, Reason} ->
           ?JXA_THROW({file_access, Reason, FileName})
   end.

-spec comp(ModuleName::atom(), Body::binary()) ->
                  {jxa_ctx:context(), binary()}.
comp(ModuleName, BinaryData) when is_binary(BinaryData) ->
    {Annots, Ast0} = jxa_parser:parse(BinaryData),
    Ctx0 = jxa_ctx:new(Annots, ModuleName, -1),
    {_, Ctx2, Binary} =
        lists:foldl(fun(DefAst, {Path, Ctx1, _Binary}) ->
                            {Ctx2, Binary1} = comp_forms(jxa_path:add(Path),
                                                         Ctx1, DefAst),
                            {jxa_path:incr(Path), Ctx2, Binary1}
                    end, {jxa_path:new(), Ctx0, <<>>}, Ast0),
    {module, ModuleName} = code:load_binary(ModuleName, "", Binary),
    {Ctx2, Binary}.

-spec format_exception(ExceptionBody::term()) -> IoList::[term()].
format_exception({file_access, enoent, FileName}) ->
    io_lib:format("File does not exist ~s", [FileName]);
format_exception({file_access, eacces, FileName}) ->
    io_lib:format("Missing  permission for reading the file: ~s",
                  [FileName]);
format_exception({file_access, eisdir, FileName}) ->
    io_lib:format("The named file is a directory: ~s",
                      [FileName]);
format_exception({file_access, enomem, FileName}) ->
    io_lib:format("There is not enough memory for the contents of the file: ~s",
                  [FileName]);
format_exception({file_access, Reason, FileName}) ->
    io_lib:format("Unexpected error (~p) attempting to read file: ~s",
                  [Reason, FileName]).

%%=============================================================================
%% Internal Functions
%%=============================================================================
%% Extract a module name from a file name
-spec extract_module_name(string()) -> ModuleName::atom().
extract_module_name(FileName) ->
    erlang:list_to_atom(filename:rootname(
                          filename:basename(FileName))).

-spec comp_forms(jxa_path:state(),
                 jxa_ctx:context(),
                 [term()]) ->
                        jxa_ctx:context().
comp_forms(Path0, Ctx0, Module = [module | _]) ->
    Ctx1 = jxa_module:comp(Path0, Ctx0, Module),
    compile_context(Ctx1);
comp_forms(Path0, Ctx0, Definition) ->
    Ctx1 = jxa_definition:comp(Path0, Ctx0, Definition),
    compile_context(Ctx1).

-spec compile_context(jxa_ctx:context()) -> jxa_ctx:context().
compile_context(Ctx0) ->
    Ctx1 = compile_module_info(Ctx0),
    Line = jxa_ctx:line(Ctx1),
    ModuleName = cerl:ann_c_atom([Line],
                                 jxa_ctx:module_name(Ctx1)),
   Exports = [cerl:ann_c_fname([ELine], Fun, Arity) ||
                 {Fun, Arity, ELine} <- jxa_ctx:exports(Ctx1)],
    Attrs = jxa_ctx:attrs(Ctx1),
    Defs = [Value || {_, Value} <-
                ec_dictionary:to_list(jxa_ctx:definitions(Ctx1))],
    {Ctx1, erl_comp(cerl:ann_c_module([Line], ModuleName,
                                      Exports, Attrs, Defs))}.

-spec compile_module_info(jxa_ctx:context()) -> jxa_ctx:context().
compile_module_info(Ctx0) ->
    ModuleName = cerl:c_atom(jxa_ctx:module_name(Ctx0)),
    ArglessBody = cerl:c_call(cerl:c_atom(erlang),
                              cerl:c_atom(get_module_info), [ModuleName]),
    Ctx1 = jxa_ctx:add_exported_definition(0, module_info, [],
                                           ArglessBody, Ctx0),
    VarName = cerl:c_var(mdetail),
    ArgBody = cerl:c_call(cerl:c_atom(erlang),
                          cerl:c_atom(get_module_info),
                          [ModuleName, VarName]),
    jxa_ctx:add_exported_definition(0, module_info, [VarName],
                                    ArgBody, Ctx1).


-spec erl_comp(cerl:cerl()) -> binary().
erl_comp(CerlAST) ->
    case compile:forms(CerlAST, [from_core,binary,no_bopt]) of
        {ok, _, Result} ->
            Result;
        Error = {error, _Errors, _Warnings} ->
            ?JXA_THROW(Error);
        InternalError ->
            ?JXA_THROW(InternalError)
    end.
