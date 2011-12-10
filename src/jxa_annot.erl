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
-module(jxa_annot).

-export([new/0,
         new_path/0,
         new_base_position/2,
         add_base_position/2,
         add/2,
         add_annot/3,
         get_annot/2]).

-define(START_CHILDREN_MARKER, -100).


-define(PATH_ROOT, -1).


%%=============================================================================
%% Types
%%=============================================================================
-type annotations() ::
        ec_dictionary:dictionary(path(), Annotation::term()).

-type path() :: integer().

%%=============================================================================
%% Internal Functions
%%=============================================================================

%% #### new_store
%%
%% We store the annotations in an ec_dictionary structure. This
%% initial dictionary is backed by ec_dict, however, this may change
%% in the future.
-spec new() -> annotations().
new() ->
    ec_dictionary:new(ec_dict).

%% #### new_path An empty path is simply a list. This function exists
%% more for documentation then anything else.
-spec new_path() -> path().
new_path() ->
   {0, []}.

-spec new_base_position(non_neg_integer(), path()) -> path().
new_base_position(Pos, {_, Path}) ->
    {Pos, Path}.

-spec add_base_position(non_neg_integer(), path()) -> path().
add_base_position(Pos, {OldPos, Path}) ->
    {Pos + OldPos, Path}.

%% #### add
%%
%% Add a new position to the path.
-spec add(non_neg_integer(), path()) -> path().
add(Position, {OldPosition, Path}) ->
    {OldPosition, [Position + OldPosition | Path]}.

%% #### add
%%
%% Given a pre created path, an annotation and the annotations object
%% we add the annotation at that path.
-spec add_annot(path(), term(), annotations()) -> annotations().
add_annot({_, Path}, Annots, Annotations) ->
    ec_dictionary:add(Path, Annots, Annotations).

%% #### get
%%
%% Given a precreated path and an annotations object we get the
%% annotation at that location.
-spec get_annot(path(), annotations()) -> term().
get_annot({_, Path}, Annotations) ->
    ec_dictionary:get(Path, Annotations).

%%=============================================================================
%% Internal Functions
%%=============================================================================
