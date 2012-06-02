-module(jxat_specs).

-export([given/3, 'when'/3, then/3]).
-include_lib("eunit/include/eunit.hrl").

given([a,module,that,declares,types], _State, _) ->
    Source = <<"(ns jxat-spec-test)

               (deftype+ tunion () (erlang/union :x :y))
               (deftype+ trange () (erlang/range 1 2))
               (deftype+ tbinary () <<>>)
               (deftype+ tbinary1 () <<8>>)
               (deftype+ tbinary2 () <<* 8>>)
               (deftype+ tbinary3 () <<7 * 8>>)
               (deftype+ tfn () (fn (...) :ok))
               (deftype+ tfn1(a) (fn (a 2) {:ok a}))
               (deftype+ tfn2() (fn () :ok))
               (deftype+ ttuple (a b) {a b 3})
               (deftype+ ttuple1 () {})
               (deftype+ tf () (fn))

               (deftype+ foo (bar baz) {bar baz})
               (deftype boo () :ok)
               (deftype+ baz () (erlang/range 1 2))
               (deftype+ hoo (a) a)
               (defspec internal-test () (foo :this :is))

               (defn internal-test ()
                {:this :is :a :test})

               (defn+ (foo :this :is) do-test1 ()
                {:this :is :a :test})

               (defn+ (boo) do-test2 (((boo) z) ((hoo :ok) y))
                {:this :is z}) ">>,

    {ok, Source}.


'when'([joxa,is,called,on,this,module], Source, _) ->
    Result = joxa.compiler:forms(Source, []),
    {ok, Result}.

then([a,beam,binary,is,produced], Ctx,  _) ->
    ?assertMatch(true, is_binary(joxa.compiler:'get-context'(result, Ctx))),
    {ok, Ctx};
then([the,described,function,can,be,called,'and',works,correctly],
     State, _) ->
    ?assertMatch([{'--joxa-info',1},
                  {'--joxa-info',2},
                  {'do-test1',0},
                  {'do-test2',2},
                  {module_info,0},
                  {module_info,1}],
                 lists:sort('jxat-spec-test':module_info(exports))),
    {ok, State}.
