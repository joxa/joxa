-module(jxat_featureful_module).

-export([given/3, 'when'/3, then/3]).

-include_lib("eunit/include/eunit.hrl").
given([a,featureful,module], _State, _) ->
    Source = <<"(module jxat-featureful
              (use string code)
              (attr sfoo 123)
              (use (lists :only (member/2 append/2)
                          :rename ((member/2 mbr))))
              (use (file :as f
                         :exclude (delete/1)
                         :rename ((change_group/2 chgrp)
                                  (change_mode/2 chmod))))
              (attr super_duper \"Hello World\")
              (require (proplists :as props))
              (require erlang code)
              (use (filename :exclude (flatten/1 append/2 join/2
                                       absname/1 absname_join/2))))">>,
    {ok, Source}.

'when'([joxa,is,called,on,this,module], State, _) ->
    Result = jxa_compile:comp("", State),
    {ok, Result}.

then([a,beam,binary,is,produced], State={_, Binary}, _) ->
    ?assertMatch(true, is_binary(Binary)),
    {ok, State};
then([the,joxa,context,for,a,featureful,module,is,correctly,formed], State={Ctx0, _}, _) ->
    validate_module(string, Ctx0),
    validate_module(code, Ctx0),
    validate_lists(Ctx0),
    validate_file(Ctx0),
    validate_filename(Ctx0),
    Required = jxa_ctx:require(Ctx0),
    Alias = jxa_ctx:alias(Ctx0),
    _Attrs = jxa_ctx:attrs(Ctx0),
    ?assertMatch(true, ec_dictionary:has_key(proplists, Required)),
    ?assertMatch(true, ec_dictionary:has_key(erlang, Required)),
    ?assertMatch(true, ec_dictionary:has_key(code, Required)),
    ?assertMatch(proplists, ec_dictionary:get(props, Alias)),
    ?assertMatch(file, ec_dictionary:get(f, Alias)),
    ?assertMatch("Hello World",
                 proplists:get_value(super_duper,
                                     'jxat-featureful':module_info(attributes))),

    ?assertMatch(123,
                 proplists:get_value(sfoo,
                                     'jxat-featureful':module_info(attributes))),
    {ok, State}.

validate_module(Module, Ctx0) ->
    %% module_info causes problems and is mostly ignored
    Exports = [El || El={Fun, _}
                         <- proplists:get_value(exports, Module:module_info()),
                     Fun =/= module_info],
    Used = jxa_ctx:use(Ctx0),
    lists:foreach(fun(Export={Fun, _}) ->
                          ?assertMatch({Fun, Module},
                                       ec_dictionary:get(Export, Used))
                  end, Exports).

validate_lists(Ctx0) ->
    Required = [{append, 2}],
    Exports = [El || El={Fun, _}
                         <- proplists:get_value(exports, lists:module_info()),
                     Fun =/= module_info],
    FilteredExports = [FunArity || FunArity <- Exports,
                                   not lists:member(FunArity, Required)],
    Used = jxa_ctx:use(Ctx0),
    lists:foreach(fun(Export={Fun, _}) ->
                          ?assertMatch({Fun, lists},
                                       ec_dictionary:get(Export, Used))
                  end, Required),

    ?assertMatch({member, lists},
                 ec_dictionary:get({mbr, 2}, Used)),

    lists:foreach(fun(Export) ->
                          ?assertThrow(not_found,
                                       ec_dictionary:get(Export, Used))
                  end, FilteredExports).

validate_file(Ctx0) ->
    DescUsed = [{{chgrp, 2}, change_group},
                {{chmod, 2}, change_mode}],
    Exports = [El || El={Fun, _}
                         <- proplists:get_value(exports, file:module_info()),
                     Fun =/= module_info],
    FilteredExports = [FunArity || FunArity <- Exports,
                                   not lists:member(FunArity,
                                                    [{delete, 1},
                                                     {change_group, 2},
                                                     {change_mode, 2}])],
    Used = jxa_ctx:use(Ctx0),
    lists:foreach(fun({Export, Target}) ->
                          ?assertMatch({Target, file},
                                       ec_dictionary:get(Export, Used))
                  end, DescUsed),
    lists:foreach(fun(Export={Fun, _}) ->
                          ?assertMatch({Fun, file},
                                       ec_dictionary:get(Export, Used))
                  end, FilteredExports).

validate_filename(Ctx0) ->
    Exports = [El || El={Fun, _}
                         <- proplists:get_value(exports, filename:module_info()),
                     Fun =/= module_info],
    DescExclude = [{absname, 1},
                   {join, 2},
                   {append, 2},
                   {flatten, 1},
                   {absname_join, 2}],
    FilteredExports = [FunArity || FunArity <- Exports,
                                   not lists:member(FunArity,
                                                    DescExclude)],
    Used = jxa_ctx:use(Ctx0),
    lists:foreach(fun(Export={Target, _}) ->
                          ?assertMatch({Target, filename},
                                       ec_dictionary:get(Export, Used))
                  end, FilteredExports),

    lists:foreach(fun(Export) ->
                          ?assertThrow(not_found,
                                       ec_dictionary:get(Export, Used))
                  end, [{absname, 1}, {absname_join, 2}]).

