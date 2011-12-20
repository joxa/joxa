-module(jxat_hello_world).

-export([given/3, 'when'/3, then/3]).
-include_lib("eunit/include/eunit.hrl").

given([a,module,that,has,a,function,that,calls,
       'io:format',on,an,argument], _State, _) ->
    Source = <<"(module helloworld
                 (use [io :only [format/2] :rename ([format/2 print])]))
               (defn+ hello-world [arg]
                 (print \"~p\" [arg]))">>,
    {ok, Source}.


'when'([joxa,is,called,on,this,module], Source, _) ->
  {ok, joxa:comp('helloworld', Source)}.

then([a,beam,binary,is,produced], State={_, Binary}, _) ->
    ?assertMatch(true, is_binary(Binary)),
    ?assertMatch([{module_info,0},{'hello-world',1},{module_info,1}],
                 helloworld:module_info(exports)),
    {ok, State};
then([the,described,function,can,be,called,'and',works,correctly],
     State, _) ->
    ?assertMatch(ok, helloworld:'hello-world'("Hello World")),
    {ok, State}.

