-module(jxat_bare_module).

-export([given/3, 'when'/3, then/3]).

-include_lib("eunit/include/eunit.hrl").

given([a,bare,module], _State, _) ->
    Module = <<"(module my-module ; Simple module test
                 )">>,
    {ok, Module}.

'when'([joxa,is,called,on,this,module], State, _) ->
    Result = joxa:comp('my-module', State),
    {ok, Result}.

then([a,beam,binary,is,produced], State={_Ctx, Binary}, _) ->
    ?assertMatch(true, is_binary(Binary)),
    ?assertMatch([{module_info,0},{module_info,1}],
                 'my-module':module_info(exports)),
    ?assertMatch([],
                 'my-module':module_info(imports)),

    {ok, State};
then([the,joxa,context,for,a,bare,module,is,correctly,formed], State={Ctx, _}, _) ->
    ?assertMatch('my-module', jxa_ctx:module_name(Ctx)),
    {ok, State}.

