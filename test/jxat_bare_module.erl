-module(jxat_bare_module).

-export([given/3, 'when'/3, then/3]).

-include_lib("eunit/include/eunit.hrl").

given([a,bare,module], _State, _) ->
    Module = <<"(ns my-module ; Simple module test
                 )">>,
    {ok, Module}.

'when'([joxa,is,called,on,this,module], State, _) ->
    Result = 'joxa-compiler':forms(State, []),
    {ok, Result}.

then([a,beam,binary,is,produced], Ctx, _) ->
    ?assertMatch(true, is_binary('joxa-cmp-ctx':'get-context'(result, Ctx))),
    ?assertMatch([{'--joxa-info',1},
                  {'--joxa-info',2},
                  {module_info,0},
                  {module_info,1}],
                 lists:sort('my-module':module_info(exports))),
    ?assertMatch([],
                 'my-module':module_info(imports)),

    {ok, Ctx};
then([the,joxa,context,for,a,bare,module,is,correctly,formed], Ctx, _) ->
    ?assertMatch('my-module', 'joxa-cmp-ctx':'get-context'('namespace-name', Ctx)),
    {ok, Ctx}.
