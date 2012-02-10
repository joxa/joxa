-module(jxat_records).

-export([given/3, 'when'/3, then/3]).
-include_lib("eunit/include/eunit.hrl").

given([a,module,that,has,defined,recordss], _State, _) ->
    Source1 = <<"
(module jxat-records-def-test)


(defrecords+ my-records (name age {sex male} {address \"unknown\"}))

(defrecords+ other one)

">>,

    {ok, Source1};
given([another,module,uses,those,recordss], Source1, _) ->
    Source2 = <<"
(module jxat-records-uses-test
       (require jxat-records-def-test))

(defn+ create1 ()
       {(jxat-records-def-test/my-records
                           \"Robert\"
                           1024
                           :male
                           \"Somewhere in Ireland\"),
       (jxat-records-def-test/my-records
                           [{:name \"Robert\"}
                           {:age 1024}
                           {:sex :male}
                           {:address \"Somewhere in Ireland\"}])})

(defn+ create2()
    {(jxat-records-def-test/other 1)
     (jxat-records-def-test/other [{:one 1}])})

(defn+ match1 (one two)
   (case one
     ((jxat-records-def-test/t two [{:name \"Robert\"}])
         :matched)
     (_
         :did-not-match)))


(defn+ match2 (one two)
   (case one
     ((jxat-records-def-test/t two [{:name \"Robert\"}
                                      {:sex :male}])
         :matched)
     (_
         :did-not-match)))


(defn+ match2 (one two)
   (case one
     ((jxat-records-def-test/t two [{:name \"Rob\"}
                                      {:sex :male}])
         :matched)
     (_
         :did-not-match)))

(defn+ with (one)
   (jxat-records-def-test/with-fields [{name local-name}
                                      {age local-age}
                                      {address local-address}]
        {local-name local-age local-address}))">>,

    {ok, {Source1, Source2}}.

'when'([joxa,is,called,on,these,modules], {Source1, Source2}, _) ->
    Result1 = joxa.compiler:forms(Source1, []),
    Result2 = joxa.compiler:forms(Source2, []),
    {ok, {Result1, Result2}}.

then([a,beam,binary,is,produced,for,both], {Ctx1, Ctx2}, _) ->
    ?assertMatch(true, is_binary(joxa.compiler:'get-context'(result, Ctx1))),
    ?assertMatch(true, is_binary(joxa.compiler:'get-context'(result, Ctx2))),
    ?assertMatch(false, joxa.compiler:'has-errors?'(Ctx1)),
    ?assertMatch(false, joxa.compiler:'has-errors?'(Ctx2)),
    {ok, ok};
then([the,described,function,can,be,called,'and',works,correctly], State, _) ->

    ?assertMatch([{'--joxa-info',1},
                  {'--joxa-info',2},

                  {module_info,0},
                  {module_info,1}],
                 lists:sort('jxat-records-def-test':module_info(exports))),
  {ok, State}.


