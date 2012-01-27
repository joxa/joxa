%%%-------------------------------------------------------------------
%%% @author Eric B Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2011, Eric B Merritt
%%% @doc
%%%
%%% @end
%%% Created : 29 Dec 2011 by Eric B Merritt <ericbmerritt@gmail.com>
%%%-------------------------------------------------------------------
-module(jxat_do_test).

%% API
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================
do_test() ->
      Source = <<"(module jxat-do-test)

                  (do
                    (defn+ do-test0 (a)
                       a)
                    (defn+ do-test1 (b)
                       b))

                  (defn+ do-test2 (c)
                      c)
                   ">>,
    {_, Binary} = joxa.compiler:forms("", Source, []),
    ?assertMatch(true, is_binary(Binary)),
    ?assertMatch([{'--joxa-info',1},
                  {'--joxa-info',2},
                  {'do-test0',1},
                  {'do-test1',1},
                  {'do-test2',1},
                  {module_info,0},
                  {module_info,1}],
                 lists:sort('jxat-do-test':module_info(exports))),
    ?assertMatch(foo, 'jxat-do-test':'do-test0'(foo)),
    ?assertMatch(bar, 'jxat-do-test':'do-test1'(bar)),
    ?assertMatch(baz, 'jxat-do-test':'do-test2'(baz)).



%%%===================================================================
%%% Support Functions
%%%===================================================================


