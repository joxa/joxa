%% -*- mode: Erlang; fill-column: 80; comment-column: 76; -*-
-module(jxa_ctx).

-export([new/1,
         new/3,
         new/4,
         annots/1,
         module_name/1,
         module_name/2,
         exports/1,
         add_export/4,
         attrs/1,
         attrs/2,
         add_attr/2,
         alias/1,
         alias/2,
         add_alias/3,
         require/1,
         require/2,
         add_require/2,
         use/1,
         use/2,
         add_use/5,
         line/1,
         line/2,
         definitions/1,
         add_exported_definition/5,
         add_definition/5]).

-export_type([context/0,
              attr/0,
              export/0,
              alias/0,
              require/0,
              use/0,
              definition/0]).

-record(context, {module_name :: atom(),
                  annots :: jxa_annot:annotations(),
                  exports :: [export()],
                  attrs :: [attr()],
                  alias :: alias(),
                  require :: require(),
                  use :: use(),
                  definitions :: definition(),
                  line :: non_neg_integer()}).

%%=============================================================================
%% Types
%%=============================================================================
-type context() :: record(context).
-type attr() :: {Key::cerl:cerl(), Value::cerl:cerl()}.
-type alias() :: ec_dictionary:dictionary(module(), module()).
-type require() :: set().
-type use_key() :: {FunctionName::atom(), arity()}.
-type use_value() :: {FunctionName::atom(), ModuleName::module()}.
-type use() :: ec_dictionary:dictionary(use_key(), use_value()).
-type definition() :: {cerl:c_fname(), cerl:c_fun()}.
-type export() :: {Fun::atom(), Arity::non_neg_integer(),
                   Line::non_neg_integer()}.
%%=============================================================================
%% Public API
%%=============================================================================

%% create a new context to use for compilation of the system to use during
%% compilation.
-spec new(jxa_annot:annotations()) -> context().
new(Annots) ->
    #context{module_name=undefined,
             attrs=[],
             exports=[],
             definitions=[],
             annots=Annots,
             alias=ec_dictionary:new(ec_dict),
             require=sets:new(),
             use=ec_dictionary:new(ec_dict)}.

-spec new(jxa_annot:annotations(),
                  module(), non_neg_integer()) -> context().
new(Annots, ModuleName, Line)
  when is_atom(ModuleName), is_integer(Line) ->
    #context{module_name=ModuleName,
             line=Line,
             annots=Annots,
             exports=[],
             attrs=[],
             definitions=[],
             alias=ec_dictionary:new(ec_dict),
             require=sets:new(),
             use=ec_dictionary:new(ec_dict)}.

%% create a new context with default values for aliased modules, required
%% modules use 'used' or imported modules.
%%
%% Aliases
%% -------
%%  This is a key value list where both the key and value are represented by
%%  atoms that represent module names. The key is the alias and the module is
%%  the aliased module.
%%
%% Requires
%% --------
%%
%% This is a list of modules that may be used in he module. This is represented
%% as a list of atoms.
%%
%% Uses
%% ----
%%
%% This is a property list of key value pairs. The key is a atom that represents
%% a function. The value is the module that contains the function.
-spec new(jxa_annot:annotations(),
                  [{module(), module()}],
                  [module()], [{use_key(), use_value()}]) ->
                         context().
new(Annots, Aliases, Requires, Uses) ->
    #context{module_name=undefined,
             annots=Annots,
             exports=[],
             attrs=[],
             definitions=[],
             alias=ec_dictionary:from_list(ec_dict, Aliases),
             require=sets:from_list(Requires),
             use=ec_dictionary:from_list(ec_dict, Uses)}.

-spec annots(context()) -> jxa_annot:annotations().
annots(#context{annots=Annots}) ->
    Annots.

-spec line(context()) -> non_neg_integer().
line(#context{line=Line}) ->
    Line.

-spec line(non_neg_integer(), context()) -> context().
line(Line, Ctx0) ->
    Ctx0#context{line=Line}.

-spec module_name(context()) -> module().
module_name(#context{module_name=ModuleName}) ->
    ModuleName.

-spec module_name(module(), context()) -> context().
module_name(Module, Ctx0) ->
    Ctx0#context{module_name=Module}.

exports(#context{exports=Exports}) ->
    Exports.

-spec add_export(Line::non_neg_integer(),
                 Fun::atom(), Arity::non_neg_integer(),
                 context()) ->
                        context().
add_export(Line, FunName, Arity, Ctx0=#context{exports=Exports}) ->
    Ctx0#context{exports=[{FunName, Arity, Line} | Exports]}.

-spec attrs(context()) -> [attr()].
attrs(#context{attrs=Attrs}) ->
    Attrs.

-spec attrs([attr()], context()) -> context().
attrs(Attrs, Ctx0) ->
    Ctx0#context{attrs=Attrs}.

-spec add_attr(attr(), context()) -> context().
add_attr({Key, Value}, Ctx0=#context{attrs=Attrs}) ->
    Ctx0#context{attrs=[{Key, Value} | Attrs]}.

-spec alias(context()) -> alias().
alias(#context{alias=Alias}) ->
    Alias.

-spec alias(alias(), context()) -> context().
alias(NewAlias, Ctx0) ->
    Ctx0#context{alias=NewAlias}.

add_alias(AliasedName, Module, Ctx0=#context{alias=Alias}) ->
    Ctx0#context{alias=ec_dictionary:add(AliasedName, Module, Alias)}.

-spec require(context()) -> require().
require(#context{require=Require}) ->
    Require.

-spec require(module(), context()) -> context().
require(Require, Ctx0) ->
    Ctx0#context{require=Require}.

-spec add_require(module(), context()) -> context().
add_require(Module, Ctx0=#context{require=Require}) ->
    Ctx0#context{require=sets:add_element(Module, Require)}.

-spec use(context()) -> use().
use(#context{use=Use}) ->
    Use.

-spec use(use(), context()) -> context().
use(Use, Ctx0) ->
    Ctx0#context{use=Use}.

-spec add_use(FunctionName::atom(), Arity::non_neg_integer(),
              TargetFun::atom(), TargetModule::module(), context()) ->
                     context().
add_use(Alias, Arity, Target, Module, Ctx0=#context{use=Use}) ->
    Ctx0#context{use=ec_dictionary:add({Alias, Arity}, {Target, Module}, Use)}.

-spec definitions(context()) -> [definition()].
definitions(#context{definitions=Definitions}) ->
    Definitions.

-spec add_exported_definition(non_neg_integer(),
                              atom(), [cerl:cerl()], cerl:cerl(),
                              context()) ->
                                     context().
add_exported_definition(Line, Name, Vars, Body, Ctx0) ->
    Arity = erlang:length(Vars),
    add_definition(Line, Name, Vars, Body,
                   add_export(Line, Name, Arity, Ctx0)).

-spec add_definition(non_neg_integer(),
                     atom(), [cerl:cerl()], cerl:cerl(),
                     context()) ->
                            context().
add_definition(Line, Name, Vars, Body, Ctx0=#context{definitions=Defs}) ->
    Arity = erlang:length(Vars),
    CerlName = cerl:ann_c_fname([Line],
                                Name, Arity),
    CerlBody = cerl:ann_c_fun([Line], Vars, Body),
    Ctx0#context{definitions=[{CerlName, CerlBody} | Defs]}.

