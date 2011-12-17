%% Path Hashing for Line and Type Information
%% ==========================================

%% We have the problem that for macros and for ease of parsing we want
%% to keep the AST as clean as possible. That is, we want it to be as
%% close to a normal list as we can get away with. However, we want to
%% be able to hang information on the graph that the AST
%% represents. Things like line numbers, additional type information
%% etc. However, in erlang we cant do that without polluting the graph
%% itself and making it harder for user written macros to be
%% implemented. So we need some way to identify specific points in the
%% graph that is the AST that we can then use as a key on this
%% additional information that we would like to show.
%%
%% In an AST nodes are identified by their location in the graph. That
%% is, every node in the graph is identified by the path leading to
%% that node.  Lets look at an example.
%%
%%     (hello world (I rock))
%%
%% In this case the 'I' node could be identified by the path [hello,
%% world, <start of children>, I]. This should be a unique identifier
%% for any point in the graph assuming that there is a single root
%% term being parsed.
%%
%% If that is true we can replace the actual elements with their
%% positions in the list. So the example above would become. [1, 3,
%% 1]. Where the outer list is 1 (everything starts at one) the 3rd
%% position is the list and the first element in that third
%% position. Lets look at something a bit more something more realistic.
%%
%%     (defn+ hello-world [foo bar] (baz bang bong))
%%
%% In this example the bang node could be identified by everything
%% leading up to it. So the path would be [defn+, hello-world,
%% <children>, <start-of_children>, bang]. Lets translate this to our
%% simple numerical paths. [1, 4, 2]. This should work to any level in
%% the graph.
%%
%% We can make it even easier to manipulate buy having the firstest
%% point in the graph be the closest point in the list so that we can
%% push onto the list as we go forward. The actual path in the example
%% above would be [2, 4, 1] and built up each time we see a list.
-module(jxa_path).

-export([new/0,
         set/2,
         incr/2,
         incr/1,
         add/1,
         add_path/1,
         path/1]).

-export_type([state/0, path/0, path_element/0]).

%%=============================================================================
%% Types
%%=============================================================================

-type state() :: {path_element(), path()}.
-type path() ::  [path_element()].
-type path_element() :: non_neg_integer().

%%=============================================================================
%% Internal Functions
%%=============================================================================

%% #### new
%% An empty path is simply a list. This function exists
%% more for documentation then anything else.
-spec new() -> state().
new() ->
   {1, []}.

-spec set(non_neg_integer(), state()) -> state().
set(Pos, {_, Path}) ->
    {Pos, Path}.

-spec incr(state()) -> state().
incr(Path) ->
    incr(1, Path).

-spec incr(non_neg_integer(), state()) -> state().
incr(Pos, {OldPos, Path}) ->
    {Pos + OldPos, Path}.

%% #### add
%%
%% Add a new position to the path.
-spec add(state()) -> state().
add({OldPosition, Path}) ->
    {1, [OldPosition | Path]}.

-spec add_path(state()) -> state().
add_path({OldPosition, Path}) ->
    [OldPosition | Path].

-spec path(state()) -> path().
path({_, Path}) ->
    Path.

%%=============================================================================
%% Tests
%%=============================================================================
-ifndef(NOTEST).
-include_lib("eunit/include/eunit.hrl").

path_test() ->
    Base = new(),
    Path0 = set(3, Base),
    Path1 = incr(Path0),
    Path2 = incr(Path1),
    Path3 = incr(4, Path2),
    Path4 = add(Path3),
    Path5 = add(incr(Path4)),
    ?assertEqual([], path(Path0)),
    ?assertEqual([], path(Path1)),
    ?assertEqual([], path(Path2)),
    ?assertEqual([9], path(Path4)),
    ?assertEqual([2,9], path(Path5)).

-endif.
