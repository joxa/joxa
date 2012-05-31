-module(jxat_records).

-export([given/3, 'when'/3, then/3]).
-include_lib("eunit/include/eunit.hrl").

given([a,module,that,has,defined,records], _State, _) ->
    Source1 = <<"
(ns jxat-records-def-test
          (require joxa.records
                   erlang lists))

(joxa.records/defrecord+ my-records name age {sex male} {address \"unknown\"})

">>,

    {ok, Source1};
given([another,module,uses,those,records], Source1, _) ->
    Source2 = <<"
(ns jxat-records-uses-test
       (require jxat-records-def-test))

(defn+ create1 ()
       {(jxat-records-def-test/make
                           \"Robert\"
                           1024
                           :male
                           \"Somewhere in Ireland\"),
       (jxat-records-def-test/make-fields
                           [{:name \"Robert\"}
                           {:age 1024}
                           {:sex :male}
                           {:address \"Somewhere in Ireland\"}])})

(defn+ match1 (one)
   (case one
     ((jxat-records-def-test/t name \"Robert\")
         :matched)
     (_
         :did-not-match)))


(defn+ match2 (one)
   (case one
     ((jxat-records-def-test/t name \"Robert\"
                               sex male)
         :matched)
     (_
         :did-not-match)))

(defn+ match3 (one)
   (case one
     ((jxat-records-def-test/t name \"Robert\"
                               sex :female)
         :matched)
     (_
         :did-not-match)))


(defn+ with (one)
   (jxat-records-def-test/let one
                              (name local-name
                               age local-age
                               address local-address)
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
                  {address,1},
                  {'address!',2},
                  {age,1},
                  {'age!',2},
                  {'element',2},
                  {'element!',3},
                  {'field-info',0},
                  {'field-info',1},
                  {'let',3},
                  {make,4},
                  {'make-fields',1},
                  {module_info,0},
                  {module_info,1},
                  {name,1},
                  {'name!',2},
                  {sex,1},
                  {'sex!',2},
                  {t,1}],
                 lists:sort('jxat-records-def-test':module_info(exports))),
    {A, B} = 'jxat-records-uses-test':create1(),
    ?assertMatch({'my-records',"Robert",1024,male,
                  "Somewhere in Ireland"}, A),
    ?assertMatch(A, B),
    ?assertMatch(matched, 'jxat-records-uses-test':match1(A)),
    ?assertMatch(matched, 'jxat-records-uses-test':match2(A)),
    ?assertMatch('did-not-match', 'jxat-records-uses-test':match3(A)),
    ?assertMatch({"Robert",1024,"Somewhere in Ireland"},
                 'jxat-records-uses-test':with(A)),
    ?assertMatch("Robert", 'jxat-records-def-test':name(B)),
    ?assertMatch(1024, 'jxat-records-def-test':age(B)),
    ?assertMatch(male, 'jxat-records-def-test':sex(B)),
    ?assertMatch("Somewhere in Ireland",
                 'jxat-records-def-test':address(B)),
    ?assertMatch({'my-records',"Brian",1024,male,
                  "Somewhere in Ireland"}, 'jxat-records-def-test':'name!'(B, "Brian")),
    ?assertMatch({'my-records',"Robert",3,male,
                  "Somewhere in Ireland"}, 'jxat-records-def-test':'age!'(B, 3)),
    ?assertMatch({'my-records',"Robert",1024,never,
                  "Somewhere in Ireland"}, 'jxat-records-def-test':'sex!'(B, never)),
    ?assertMatch({'my-records',"Robert",1024,male,
                  "wanderer"},
                 'jxat-records-def-test':'address!'(B, "wanderer")),
  {ok, State}.


