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
      Source = <<"(ns jxat-incremental-test
                    (require io)
                    (use (erlang :only (==/2 and/2))))

                (defspec post-test () (erlang/term))

                (defn+ do-test (a)
                    ((post-test) a))

                (defn post-test ()
                    (fn (foo) foo))">>,
    Ctx = 'joxa-compiler':forms(Source, []),
    ?assertMatch(true, is_binary('joxa-compiler':'get-context'(result, Ctx))),
    ?assertMatch(1, 'jxat-incremental-test':'do-test'(1)),
    ?assertMatch(foo, 'jxat-incremental-test':'do-test'(foo)),
    ?assertMatch(bar, 'jxat-incremental-test':'do-test'(bar)).


incremental_fail_test() ->
      Source = <<"(ns jxat-incremental-fail-test
                    (require io)
                    (use (erlang :only (==/2 and/2))))

                (defspec post-test () (erlang/term))

                (defn+ do-test (a)
                    ((post-test) a))">>,
    RawCtx = 'joxa-compiler':forms(Source, []),
    ?assertMatch(true,
                 'joxa-compiler':'has-errors?'(RawCtx)),
    ?assertMatch([{{'undefined-functions',[{'post-test',0}]},
                   {[],{0,0}}}],
                 'joxa-compiler':'get-context'(errors, RawCtx)).




%%%===================================================================
%%% Support Functions
%%%===================================================================


