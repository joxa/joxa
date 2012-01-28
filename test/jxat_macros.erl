-module(jxat_macros).

-export([given/3, 'when'/3, then/3]).
-include_lib("eunit/include/eunit.hrl").

given([a,module,that,contains,macros], _State, _) ->
    Source = <<"(module jxat-macro-test
                    (require erlang))

                (defmacro+ test1 (foo a)
                    :ok)
;                   `(defun+ ~foo ()
;                        (erlang/+ 22 ~a))

                (defmacro+ test2 ()
                    :ok)
;                   `(do (test1 gen-test1 22)
;                        (defn gen-test2 ()
;                            :ok)))

;               (test2)

                 (defmacro+ when (test &rest body)
                   :ok)
;                       `(case ~test
;                           (:true
;                               ~@body)
;                           (:false
;                               :ok)
;                           (_
;                                (erlang/throw :invalid-when))))

                (defn+ test3 ()
                    (when :true
                        :got-it)) ">>,
    {ok, Source}.

'when'([joxa,is,called,on,this,module], Source, _) ->
    Result = joxa.compiler:forms("", Source, []),
    {ok, Result}.

then([a,beam,binary,is,produced],  State={_, Binary}, _) ->
    ?assertMatch(true, is_binary(Binary)),
    {ok, State};
then([the,described,function,can,be,called,'and',works,correctly], State, _) ->
    ?assertMatch([{'--joxa-info',1},
                  {'--joxa-info',2},
                  {module_info,0},
                  {module_info,1},
                  {'test1',2},
                  {'test2',0},
                  {'test3',0},
                  {'when',2}],
                 lists:sort('jxat-macro-test':module_info(exports))),
    ?assertMatch([{test1,2}, {test2,0}, {'when',2}],
    lists:sort('jxat-macro-test':'--joxa-info'(macro))),
    ?assertMatch(true, 'jxat-macro-test':'--joxa-info'(macro, {test1, 2})),
    ?assertMatch(true, 'jxat-macro-test':'--joxa-info'(macro, {test2, 0})),
    ?assertMatch(true, 'jxat-macro-test':'--joxa-info'(macro, {'when', 2})),
    ?assertMatch(false, 'jxat-macro-test':'--joxa-info'(macro, {test3, 0})),
%    ?assertMatch(44, 'jxat-macro-test':'gen-test1'()),
%    ?assertMatch(ok, 'jxat-macro-test':'gen-test2'()),
%    ?assertMatch('got-it', 'jxat-macro-test':'do-test1'()),
    {ok, State}.

