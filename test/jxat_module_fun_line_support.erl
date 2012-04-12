-module(jxat_module_fun_line_support).

-export([given/3, 'when'/3, then/3]).
-include_lib("eunit/include/eunit.hrl").

given([a,module,that,has,a,function,that,calls,
       module], _State, _) ->
    Source = <<"
(module jxat-module-fun)

   (defn+ get-module ()
     ($module-name))

   (defn+ get-fun()
     ($function-name))

   (defn+ get-line()
     ($line-number))

   (defn+ test-case ()
     (case (get-module)
       (:jxat-module-fun
           :ok))
     (case (get-fun)
       (:get-fun
           :ok))
     (case (get-line)
       (11
        :ok)))
">>,
    {ok, Source}.


'when'([joxa,is,called,on,this,module], Source, _) ->
  {ok, joxa.compiler:forms(Source, [])}.

then([a,beam,binary,is,produced], Ctx, _) ->
    ?assertMatch(false, 'joxa.compiler':'has-errors?'(Ctx)),
    ?assertMatch(true, is_binary(joxa.compiler:'get-context'(result, Ctx))),
    ?assertMatch([{'--joxa-info',1},
                  {'--joxa-info',2},
                  {'get-fun',0},
                  {'get-line',0},
                  {'get-module',0},
                  {module_info,0},
                  {module_info,1},
                  {'test-case',0}],
                 lists:sort('jxat-module-fun':module_info(exports))),
    {ok, Ctx};
then([the,described,function,returns,the,name,'of',the,module],
     State, _) ->
    ?assertMatch('jxat-module-fun', 'jxat-module-fun':'get-module'()),
    ?assertMatch('get-fun', 'jxat-module-fun':'get-fun'()),
    ?assertMatch(11, 'jxat-module-fun':'get-line'()),
    'jxat-module-fun':'test-case'(),
    {ok, State}.

