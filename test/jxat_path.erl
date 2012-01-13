%%%-------------------------------------------------------------------
%%% @author Eric B Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2011, Eric B Merritt
%%% @doc
%%%
%%% @end
%%% Created : 29 Dec 2011 by Eric B Merritt <ericbmerritt@gmail.com>
%%%-------------------------------------------------------------------
-module(jxat_path).

%% API
-include_lib("eunit/include/eunit.hrl").


%%%===================================================================
%%% Tests
%%%===================================================================
path_test() ->
    Base = joxa.compiler:'new-path'(),
    Path1 = joxa.compiler:'incr-path'(Base),
    Path2 = joxa.compiler:'incr-path'(Path1),
    Path3 = joxa.compiler:'incr-path'(4, Path2),
    Path4 = joxa.compiler:'traverse-path'(Path3),
    Path5 = joxa.compiler:'incr-path'(Path4),
    ?assertEqual([], joxa.compiler:'path?'(Path1)),
    ?assertEqual([], joxa.compiler:'path?'(Path2)),
    ?assertEqual([7], joxa.compiler:'path?'(Path4)),
    ?assertEqual([2,7], joxa.compiler:'traverse-and-get-path'(Path5)).

