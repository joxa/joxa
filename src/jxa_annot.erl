%% Path Hashing for Line and Type Information
%% ==========================================
-module(jxa_annot).

-export([new/0,
         add/3,
         get/2]).

%%=============================================================================
%% Types
%%=============================================================================
-type annotations() ::
        ec_dictionary:dictionary(jxa_path:path(), Annotation::term()).

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

%% #### add
%%
%% Given a pre created path, an annotation and the annotations object
%% we add the annotation at that path.
-spec add(jxa_path:path(), term(), annotations()) -> annotations().
add(Path, Annots, Annotations) ->
    ec_dictionary:add(Path, Annots, Annotations).

%% #### get
%%
%% Given a precreated path and an annotations object we get the
%% annotation at that location.
-spec get(jxa_path:path(), annotations()) -> term().
get(Path, Annotations) ->
    ec_dictionary:get(Path, Annotations).

%%=============================================================================
%% Internal Functions
%%=============================================================================
