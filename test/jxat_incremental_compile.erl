%%%-------------------------------------------------------------------
%%% @author Eric B Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2011, Eric B Merritt
%%% @doc
%%%
%%% @end
%%% Created : 29 Dec 2011 by Eric B Merritt <ericbmerritt@gmail.com>
%%%-------------------------------------------------------------------
-module(jxat_incremental_compile).

%% API
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================
incremental_test() ->
      Source = <<"(module jxat-incremental-test
                    (require io)
                    (use (erlang :only (==/2 and/2))))

                (defspec post-test () (erlang/term))

                (defn+ do-test (a)
                    ((post-test) a))

                (defn post-test ()
                    (fn (foo) foo))">>,
    {_, Binary} = joxa.compiler:forms(Source, []),
    ?assertMatch(true, is_binary(Binary)),
    ?assertMatch(1, 'jxat-incremental-test':'do-test'(1)),
    ?assertMatch(foo, 'jxat-incremental-test':'do-test'(foo)),
    ?assertMatch(bar, 'jxat-incremental-test':'do-test'(bar)).


incremental_fail_test() ->
      Source = <<"(module jxat-incremental-fail-test
                    (require io)
                    (use (erlang :only (==/2 and/2))))

                (defspec post-test () (erlang/term))

                (defn+ do-test (a)
                    ((post-test) a))">>,
    ?assertThrow('unresolved-function-dependencies',
                 joxa.compiler:forms(Source, [])).



%%%===================================================================
%%% Support Functions
%%%===================================================================


