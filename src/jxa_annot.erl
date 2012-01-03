%% Path Hashing for Line and Type Information
%% ==========================================
-module(jxa_annot).

-export([new/1,
         add/4,
         get/2,
         get_type/2,
         get_line/2,
         get_line/3,
         get_idx/2]).

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
-spec new(string()) -> annotations().
new(Filename) ->
    ec_dictionary:add('__filename__', Filename, ec_dictionary:new(ec_dict)).

%% #### add
%%
%% Given a pre created path, an annotation and the annotations object
%% we add the annotation at that path.
-spec add(jxa_path:path(), term(), term(),
          annotations()) -> annotations().
add(Path, Type, Idx={Line, _}, Annotations) ->
    Filename = ec_dictionary:get('__filename__', Annotations),
    ec_dictionary:add(Path, {Type, Idx, [Line, {file, Filename}]},
                      Annotations).

%% #### get
%%
%% Given a precreated path and an annotations object we get the
%% annotation at that location.
-spec get(jxa_path:path(), annotations()) -> term().
get(Path, Annotations) ->
    ec_dictionary:get(Path, Annotations).

get_line(Path, Annotations) ->
    {_, _, LineAnnotations} = jxa_annot:get(Path, Annotations),
    LineAnnotations.

get_line(Path, Extra, Annotations) when is_list(Extra) ->
    {_, _, LineAnnotations} = jxa_annot:get(Path, Annotations),
    Extra ++ LineAnnotations;
get_line(Path, Extra, Annotations) ->
    {_, _, LineAnnotations} = jxa_annot:get(Path, Annotations),
    [Extra | LineAnnotations].

get_idx(Path, Annotations) ->
    {_, Idx, _} = jxa_annot:get(Path, Annotations),
    Idx.

get_type(Path, Annotations) ->
    {Type, _, _} = ec_dictionary:get(Path, Annotations),
    Type.

%%=============================================================================
%% Internal Functions
%%=============================================================================
