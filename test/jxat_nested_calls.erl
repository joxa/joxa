%%%-------------------------------------------------------------------
%%% @author Eric B Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2011, Eric B Merritt
%%% @doc
%%%
%%% @end
%%% Created : 29 Dec 2011 by Eric B Merritt <ericbmerritt@gmail.com>
%%%-------------------------------------------------------------------
-module(jxat_nested_calls).

%% API
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================
nested_calls_test() ->
      Source = <<"(ns jxat-nested-calls-test
                    (require io)
                    (use (erlang :only (==/2 and/2))))

                (defn internal-test ()
                    (fn (foo) foo))

                (defn+ do-test (a)
                    ((internal-test) a))">>,
    Ctx = joxa.compiler:forms(Source, []),
    ?assertMatch(true, is_binary(joxa.compiler:'get-context'(result, Ctx))),
    ?assertMatch(1, 'jxat-nested-calls-test':'do-test'(1)),
    ?assertMatch(foo, 'jxat-nested-calls-test':'do-test'(foo)),
    ?assertMatch(bar, 'jxat-nested-calls-test':'do-test'(bar)).



%%%===================================================================
%%% Support Functions
%%%===================================================================


