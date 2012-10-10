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
    Base = 'joxa-cmp-path':'new-path'(),
    Path1 = 'joxa-cmp-path':'incr-path'(Base),
    Path2 = 'joxa-cmp-path':'incr-path'(Path1),
    Path3 = 'joxa-cmp-path':'incr-path'(4, Path2),
    Path4 = 'joxa-cmp-path':'traverse-path'(Path3),
    Path5 = 'joxa-cmp-path':'incr-path'(Path4),
    ?assertEqual([], 'joxa-cmp-path':'path?'(Path1)),
    ?assertEqual([], 'joxa-cmp-path':'path?'(Path2)),
    ?assertEqual([7], 'joxa-cmp-path':'path?'(Path4)),
    ?assertEqual([2,7], 'joxa-cmp-path':'traverse-and-get-path'(Path5)).
