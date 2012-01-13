-module(jxat_specs).

-export([given/3, 'when'/3, then/3]).
-include_lib("eunit/include/eunit.hrl").

given([a,module,that,declares,types], _State, _) ->
    Source = <<"(module jxat-spec-test)

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
    Result = jxa_compile:comp("", Source),
    {ok, Result}.

then([a,beam,binary,is,produced], State = {_, Binary},  _) ->
    ?assertMatch(true, is_binary(Binary)),
    {ok, State};
then([the,described,function,can,be,called,'and',works,correctly],
     State = {_, _Binary}, _) ->
    ?assertMatch([{'do-test1',0},
                  {'do-test2',2},
                  {module_info,0},
                  {module_info,1}],
                 lists:sort('jxat-spec-test':module_info(exports))),
    {ok, State}.

%% has_spec(Beam, FA0)->
%%     {ok,{_,[{abstract_code,{_,AC}}]}} =
%%         beam_lib:chunks(Beam,[abstract_code]),
%%     lists:any(fun({attribute,_,'spec',{FA1,_}}) ->
%%                       FA0 == FA1;
%%                  (_) ->
%%                       false
%%               end, AC).

%% has_type(Beam, {F, A})->
%%     {ok,{_,[{abstract_code,{_,AC}}]}} =
%%         beam_lib:chunks(Beam,[abstract_code]),
%%     lists:any(fun({attribute,_,'type',{Fun, _, Args}}) ->
%%                          F == Fun andalso
%%                              erlang:length(Args) == A;
%%                  (_) ->
%%                       false
%%               end, AC).


%% has_exported_type(Beam, FA0)->
%%     {ok,{_,[{abstract_code,{_,AC}}]}} =
%%         beam_lib:chunks(Beam,[abstract_code]),
%%     NTypes = lists:foldl(fun({attribute,_,'export_type', Types}, _) ->
%%                                  Types;
%%                             (_, Acc) ->
%%                                  Acc
%%                          end, none, AC),
%%     lists:member(FA0, NTypes).

