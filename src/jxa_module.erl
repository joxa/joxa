%% -*- mode: Erlang; fill-column: 76; comment-column: 76; -*-
%% Joxa Module Definitions
%% =======================
%%
%% Module declarations in Joxa are more complex then module definitions in
%% Erlang. They follow the Clojure model much more closely then the Erlang
%% module. That is that all modules used in the system must be required. You
%% may provide an alias for a dependent module name in both the require
%% clause and the use clauses. You may also import functions from modules in
%% the use clause.
%%
%% At the moment paramaratized modules are not supported. There may be
%% mulitiple

-module(jxa_module).

-include_lib("joxa/include/joxa.hrl").
-export([comp/3, format_exception/1]).

%%=============================================================================
%% Public API
%%=============================================================================
format_exception({invalid_module_declaration, {Line, _}}) ->
    io_lib:format("Invalid module declaration at line ~p", [Line]);
format_exception({invalid_module_declaration, -1}) ->
    "Invalid module declaration";
format_exception({invalid_require_clause, {Line, Col}}) ->
    io_lib:format("Invalid require clause in module declaration ~p:~p",
                  [Line, Col]);
format_exception({invalid_use, non_existant_fun_name, {Line, Column}}) ->
    io_lib:format("Function specified that does not exist in use clause "
                  "in module declaration ~p:~p",
                  [Line, Column]);
format_exception({invalid_use, invalid_fun_spec, {Line, Column}}) ->
    io_lib:format("Malformed function specification at ~p:~p",
                  {Line, Column});
format_exception({invalid_module, Module, {Line, Column}}) ->
    io_lib:format("Invalid module (~p) specified at ~p:~p",
                  [Module, Line, Column]).


%% Module Form
%% -----------
%% The form of the module is defined as follows.
%%
%%     (module <module_name>
%%        <require>
%%        <use>
%%        <attributes>)
%%
%% The module clause is a special form and its contents are not evaluated.
%%
%% comp compiles the provided AST into a Joxa context. Later that context
%% must be compiled to core erlang.
-spec comp(jxa_path:state(), jxa_ctx:context(), jxa_parser:ast()) ->
                  xa_ctx:context().
comp(Path0, Ctx0, [module, ModuleName | Rest])
  when is_atom(ModuleName) ->
    Path1 = jxa_path:add(Path0),
    case jxa_annot:get(jxa_path:path(Path1), jxa_ctx:annots(Ctx0)) of
        {ident, {Line, _}} ->
            {_, Ctx3} =
                lists:foldl(
                  fun(Form, {Path2, Ctx1}) ->
                          Ctx2 =
                          comp_body(jxa_path:add(Path2),
                                    jxa_ctx:line(Line,
                                                 Ctx1), Form),
                      {jxa_path:incr(Path2), Ctx2}
                  end, {jxa_path:incr(2, Path0),
                        jxa_ctx:module_name(ModuleName,
                                            Ctx0)}, Rest),
            Ctx3;
        {_, Idx} ->
            ?JXA_THROW({invalid_module_declaration, Idx});
        _ ->
            ?JXA_THROW({invalid_module_declaration, -1})
    end.

%% Module Body
%% ----------
%%
%%  The module body may consist of any number of require, use are attribute
%%  clauses in any order. Each clause starts with a (require ...)
%%  (use ....) or (attr ...)
%%
-spec comp_body(jxa_path:state(), jxa_ctx:context(), term()) ->
                       jxa_ctx:context().
comp_body(_Path0, Ctx0, []) ->
    Ctx0;
comp_body(Path0, Ctx0, [require | ReqBody]) ->
    comp_require(jxa_path:incr(1, Path0), Ctx0, ReqBody);
comp_body(Path0, Ctx0, [attr | AttrBody]) ->
    comp_attr(jxa_path:incr(1, Path0), Ctx0, AttrBody);
comp_body(Path0, Ctx0, [use | UseBody]) ->
    {_, Idx} = jxa_annot:get(jxa_path:path(Path0), jxa_ctx:annots(Ctx0)),
    comp_use(Idx, Ctx0, UseBody, {undefined, []});
comp_body(Path0, Ctx0, _) ->
    {_, Idx} = jxa_annot:get(jxa_path:path(Path0), jxa_ctx:annots(Ctx0)),
    ?JXA_THROW({invalid_form, Idx}).



%% Require Clause
%% --------------
%%
%% The require clause is a list that starts with the ident 'require' and
%% then contains a require body. Examples of the require and require bodies
%% appear below.
%%
%% (require (erlang string test))
%% (require string [test :as test])
%% (require [string :as str])
%% (require [string :as str1])
%% (require string test)
%%
%% The body of the require contains a list of require clauses, in the form
%%
%%    (erlang string test)
%%    (string [test :as test])
%%
%%  or a simple group of require clauses in the form:
%%
%%    string test
%%    [string :as str] compiler
%%
%% Each clause is a module name or a module alias form in the form:
%%
%%    [string :as str]
%%
-spec comp_require(jxa_path:state(), jxa_ctx:context(),
                  RequireClause::term()) -> jxa_ctx:context().
comp_require(_Path0, Ctx0, []) ->
    Ctx0;
comp_require(Path0, Ctx0, [Module | Rest]) when is_atom(Module) ->
    try
        Module:module_info()
    catch
        error:undef ->
            Path1 = jxa_path:add(Path0),
            {_, Idx} = jxa_annot:get(jxa_path:path(Path1),
                                     jxa_ctx:annots(Ctx0)),
            ?JXA_THROW({invalid_require_clause, {bad_module, Module}, Idx})
    end,
    Ctx1 = jxa_ctx:add_require(Module, Ctx0),
    comp_require(jxa_path:incr(Path0), Ctx1, Rest);
comp_require(Path0, Ctx0, [[Module, [quote, as], ModuleAlias] | Rest])
  when is_atom(Module), is_atom(ModuleAlias) ->
    try
        Module:module_info()
    catch
        error:undef ->
            Path1 = jxa_path:add(Path0),
            {_, Idx} = jxa_annot:get(jxa_path:path(Path1),
                                     jxa_ctx:annots(Ctx0)),
            ?JXA_THROW({invalid_require_clause, {bad_module, Module}, Idx})
    end,
    Ctx1 = jxa_ctx:add_alias(ModuleAlias, Module,
                             jxa_ctx:add_require(Module, Ctx0)),
        comp_require(jxa_path:incr(Path0), Ctx1, Rest);
comp_require(Path0, Ctx0, _Invalid) ->
    {_, Idx} = jxa_annot:get(jxa_path:add_path(Path0), jxa_ctx:annots(Ctx0)),
    ?JXA_THROW({invalid_require_clause, Idx}).

%% Attribute Clauses
%% -----------------
%%
%% Attribute clauses are the simplest of the three clauses There are simply
%% a three element list where the first element is the ident 'attr', the
%% second element is a Joxa term that provides the key value and the third
%% is a Joxa term that provides the value.
%%
%% Attributes follow the form:
%%
%%     (attr <key> <value>)
%%
-spec comp_attr(jxa_path:state(), jxa_ctx:context(), term()) ->
                       jxa_ctx:context().
comp_attr(Path0, Ctx0, [Key, Value]) ->
    jxa_ctx:add_attr({jxa_literal:comp(jxa_path:add(Path0), Ctx0, Key),
                      jxa_literal:comp(jxa_path:add(jxa_path:incr(Path0)),
                                       Ctx0, Value)},
                     Ctx0);
comp_attr(Path0, Ctx0, _) ->
    {_, Idx} = jxa_annot:get(jxa_path:add_path(Path0), jxa_ctx:annots(Ctx0)),
    ?JXA_THROW({invalid_attr_clause, Idx}).

%% Use Clauses
%% -----------
%%
%% At last we get to the use clauses. Use clauses are, by far, the most
%% complex of the header clauses as the both manipulate and subset the
%% functions buing used while at the same time aliasing the function if
%% required.
%%
%%     (use string)
%%     (use [string :only [tokens/2]])
%%     (use [string :exclude [substr/3 join/2 join/3]])
%%     (use [string :rename ([substr/3 str-substring] [join/2 str-join])])
%%     (use [string :as str :only [join/2 substr/3]])
%%     (use [string :as str :only [tokens/2]])
%%     (use [string :as str :exclude [substr/3 join/2 join/3]])
%%     (use [string :as str :rename ([substr/3 str-substring] [join/2 str-join])])
%%
%% As you can see each clause may consist of a module name, or a vector/list
%% that contains a few some clauses.  The subcluase is always headed by a
%% module name, followed by an action, followed by the subject of that
%% action. The subclause action/subject may ocurre in any order. Even though
%% some do not make sense when used together. So, for example you could have
%% the following
%%
%%     (use [string :rename ([substr/3 str-substring] [join/2 str-join]
%%                  :exclude [substr/4 join/2]
%%                  :as str)])
%%
%% This would be perfectly valid and could use occur in any order at all.
-spec comp_use(jxa_parser:index(), jxa_ctx:context(), term(),
              {ModuleName::atom(),
               Imports::[{FunName::atom(), Arity::non_neg_integer()}]}) ->
                      jxa_ctx:context().

comp_use(_Idx, Ctx0, [], {ModuleName, Exports}) ->
    populate_use_context({ModuleName, Exports}, Ctx0);
comp_use(Idx, Ctx0, [[ModuleName | ClauseBody] | Rest], _Acc)
  when is_atom(ModuleName) ->
    try
        ModuleName:module_info()
    catch
        error:undef ->
            ?JXA_THROW({invalid_use_clause, {bad_module, ModuleName}, Idx})
    end,
    Ctx1 = jxa_ctx:add_require(ModuleName, Ctx0),
    Exports = get_exports(ModuleName, Idx),
    Ctx2 = handle_use_clauses(Idx, Ctx1, ClauseBody, {ModuleName, Exports}),
    comp_use(Idx, Ctx2, Rest, {undefined, []});
comp_use(Idx, Ctx0, [ModuleName | Rest], _Acc)
  when is_atom(ModuleName) ->
    Exports = get_exports(ModuleName, Idx),
    comp_use(Idx, populate_use_context({ModuleName, Exports}, Ctx0),
             Rest, {undefined, []}).


-spec handle_use_clauses(jxa_parser:index(), jxa_ctx:content(),
                         [term()], {ModuleName::atom(),
                                    Exports::[{AliasFun::atom(),
                                               Arity::non_neg_integer(),
                                               AliasFun::atom()}]}) ->
                                jxa_ctx:context().
handle_use_clauses(_Idx, Ctx0, [], Acc) ->
    populate_use_context(Acc, Ctx0);
handle_use_clauses(Idx, Ctx0, [[quote, as], AliasName | Rest], Acc = {ModuleName, _}) ->
    Ctx1 = jxa_ctx:add_alias(AliasName, ModuleName, Ctx0),
    handle_use_clauses(Idx, Ctx1, Rest, Acc);
handle_use_clauses(Idx, Ctx0, [[quote, only], TargetFuns | Rest],
                   {ModuleName, Exports0})
  when is_atom(ModuleName), is_list(TargetFuns) ->
    Specs = gather_fun_arity_pairs(Idx, TargetFuns, []),
    Exports1 = lists:foldl(fun({Fun, Arity}, Acc) ->
                                   El = {{Fun, Arity}, Fun},
                                   case lists:member(El, Exports0) of
                                       true ->
                                           [El | Acc];
                                       false ->
                                           ?JXA_THROW({invalid_use,
                                                       non_existant_fun_name,
                                                       Idx})
                                   end
                           end, [], Specs),
    handle_use_clauses(Idx, Ctx0, Rest, {ModuleName, Exports1});
handle_use_clauses(Idx, Ctx0, [[quote, exclude], TargetFuns | Rest],
                   {ModuleName, Exports0})
  when is_atom(ModuleName), is_list(TargetFuns) ->
    Specs = gather_fun_arity_pairs(Idx, TargetFuns, []),
    Exports1 = lists:foldl(fun(El = {{Fun, Arity}, _}, Acc) ->
                                   El = {{Fun, Arity}, Fun},
                                   case lists:member({Fun, Arity}, Specs) of
                                       true ->
                                           Acc;
                                       false ->
                                           [El | Acc]
                                   end
                           end, [], Exports0),
    handle_use_clauses(Idx, Ctx0, Rest, {ModuleName, Exports1});
handle_use_clauses(Idx, Ctx0, [[quote, rename], TargetFuns | Rest],
                   {ModuleName, Exports0})
  when is_atom(ModuleName), is_list(TargetFuns) ->
    Specs = gather_fun_alias_pairs(Idx, TargetFuns, []),
    Exports2 =
        lists:foldl(fun({{Fun, Arity}, Alias}, Exports1) ->
                            lists:keyreplace({Fun, Arity}, 1, Exports1,
                                             {{Fun, Arity}, Alias})
                    end, Exports0, Specs),
    handle_use_clauses(Idx, Ctx0, Rest, {ModuleName, Exports2});
handle_use_clauses(Idx, Ctx0, ProbablyMoreSpecs, Acc) ->
    comp_use(Idx, Ctx0, ProbablyMoreSpecs, Acc).

-spec populate_use_context({ModuleName::atom(),
                            Exports::[{Fun::atom(),
                                       Arity::non_neg_integer()}]},
                          jxa_ctx:context()) ->
                                  jxa_ctx:context().
populate_use_context({undefined, []}, Ctx0) ->
    Ctx0;
populate_use_context({ModuleName, Imports}, Ctx0) ->
    lists:foldl(fun({{Name, Arity}, AliasName}, Ctx1) ->
                        jxa_ctx:add_use(AliasName, Arity, Name, ModuleName, Ctx1)
                end, Ctx0, Imports).

%% A Function ref in the module declaration looks as follows
%%
%%     fun_name/3
%%
%% When this is parsed it is actually parsed into three seperate elements. the
%% atom 'fun_name' the atom '/' and finially the atom 3, this function takes a
%% form that looks like this:
%%
%%     [substr/3 str-substring]
%%
%% that is parsed into this [{substr, 3}, 'sub-string'] into a something more
%% usable in erlang ie
%%
%%     [{substr, 3}, str-substring]
%%
-spec gather_fun_alias_pairs(jxa_parser:index(),
                             [atom() | non_neg_integer()],
                             [{{FunName::atom(), Arity::non_neg_integer()},
                               Alias::atom()}]) ->
                                    [{{FunName::atom(),
                                       Arity::non_neg_integer()},
                                      Alias::atom()}].
gather_fun_alias_pairs(Idx, [[{'__fun__', Fun, Arity}, Alias] | Rest], Acc)
  when is_atom(Alias) ->
    gather_fun_alias_pairs(Idx,  Rest, [{{Fun, Arity}, Alias} | Acc]);
gather_fun_alias_pairs(_Idx, [], Acc) ->
    Acc;
gather_fun_alias_pairs(Idx, _Detail, _) ->
    ?JXA_THROW({invalid_use, invalid_fun_spec, Idx}).

%% Similar to gather_fun_alias_pairs gather_fun_arity_pairs parses fun refs
%% in the form of fun_name/3 that get parsed into {'fun_name', 3}.
-spec gather_fun_arity_pairs(jxa_parser:index(),
                             [atom() | non_neg_integer()],
                             [{FunName::atom(), Arity::non_neg_integer()}]) ->
                                    [{FunName::atom(),
                                      Arity::non_neg_integer()}].
gather_fun_arity_pairs(Idx, [{'__fun__', Fun, Arity} | Rest], Acc) ->
    gather_fun_arity_pairs(Idx,  Rest, [{Fun, Arity} | Acc]);
gather_fun_arity_pairs(_Idx, [], Acc) ->
    Acc;
gather_fun_arity_pairs(Idx, _Detail, _Acc) ->
    ?JXA_THROW({invalid_use, invalid_fun_spec, Idx}).

-spec get_exports(ModuleName::atom(), jxa_parser:index()) ->
                         [{FunName::atom(), Arity::non_neg_integer()}].
get_exports(Module, Idx) ->
    case proplists:get_value(exports, Module:module_info()) of
        undefined ->
            ?JXA_THROW({undefined_module, Module, Idx});
        Exports ->
            [{{Fun, Arity}, Fun} || {Fun, Arity} <- Exports]
    end.

%%=============================================================================
%% Unit tests
%%=============================================================================
-ifndef(NOTEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
