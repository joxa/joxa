-module(jxat_module_info).

-export([given/3, 'when'/3, then/3]).
-include_lib("eunit/include/eunit.hrl").

given([a,module,that,with,a,valid,module,but,broken,body], _State, _) ->
  Source = <<"(module jxat-case-test
                    (require jxat_module_info)
                    (use (erlang :only (==/2 phash2/1 and/2))))

                (defn internal-test (arg1 arg2)
                 BORKED BORKED BORKED
                (defn internal-test2 (arg1 arg2 arg3)
                     (phash2 :bar))">>,
    {ok, Source};
given([a,module,that,has,a,require,'and',use], _State, _) ->
  Source = <<"(module jxat-case-test
                    (require jxat_module_info)
                    (use (erlang :only (==/2 phash2/1 and/2))))

                (defn internal-test (arg1 arg2)
                            :ok)
                (defn internal-test2 (arg1 arg2 arg3)
                     (phash2 :bar))">>,
    {ok, Source}.

'when'([joxa,info,is,called,on,this,module], Source, _) ->
    Result = joxa.compiler:info(Source, []),
    {ok, Result}.

then([context,is,produced], Ctx, _) ->
    ?assertMatch(context, element(1, Ctx)),
    {ok, Ctx};
then([context,contains,the,required,information], Ctx, _) ->
    Req = joxa.compiler:'get-context'(requires, Ctx),
    ?assertMatch([erlang,jxat_module_info], ec_dictionary:keys(Req)).



