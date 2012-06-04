%%%-------------------------------------------------------------------
%%% @author Eric B Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2011, Eric B Merritt
%%% @doc
%%%
%%% @end
%%% Created : 29 Dec 2011 by Eric B Merritt <ericbmerritt@gmail.com>
%%%-------------------------------------------------------------------
-module(jxat_core_add).

%% API
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================
add_sub_test() ->
      Source = <<"(ns jxat-core-add-test
                    (require io)
                    (use (joxa.core :only (+/1 -/1))))

                (defn+ do-add ()
                    (+ 1 2 3 4 5 6 7 8 9 10))

                (defn+ do-sub ()
                    (- 1 2 3 4 5 6 7 8 9 10))">>,
    Ctx = joxa.compiler:forms(Source, []),
    ?assertMatch(true,is_binary(joxa.compiler:'get-context'(result, Ctx))),
    ?assertMatch(1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10,
                 'jxat-core-add-test':'do-add'()),
    ?assertMatch(1 - 2 - 3 - 4 - 5 - 6 - 7 - 8 - 9 - 10,
                 'jxat-core-add-test':'do-sub'()).

%%%===================================================================
%%% Support Functions
%%%===================================================================
