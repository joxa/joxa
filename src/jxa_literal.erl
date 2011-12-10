%% -*- mode: Erlang; fill-column: 80; comment-column: 76; -*-
-module(jxa_literal).

-export([comp/3]).

%%=============================================================================
%% Types
%%=============================================================================

%%=============================================================================
%% Public API
%%=============================================================================
-spec comp(jxa_annot:path(), jxa_ctx:context(), term()) -> cerl:cerl().
comp(Path0, Ctx0, Symbol) when is_atom(Symbol) ->
    {_, {Line, _}} = jxa_annot:get_annot(Path0, jxa_ctx:annots(Ctx0)),
    cerl:ann_c_atom([Line], Symbol);
comp(Path0, Ctx0, Integer) when is_integer(Integer) ->
    {Type, {Line, _}} = jxa_annot:get_annot(Path0, jxa_ctx:annots(Ctx0)),
    case Type of
        integer ->
            cerl:ann_c_int([Line], Integer);
        char ->
            cerl:ann_c_char([Line], Integer)
    end;
comp(Path0, Ctx0, Float) when is_float(Float) ->
    {float, {Line, _}} = jxa_annot:get_annot(Path0, jxa_ctx:annots(Ctx0)),
    cerl:ann_c_float([Line], float);
comp(Path0, Ctx0, Element) when is_list(Element) ->
    {Type, {Line, _}} = jxa_annot:get_annot(Path0, jxa_ctx:annots(Ctx0)),
    case Type of
        list ->
            comp_list(Path0, Line, Ctx0, Element);
        vector ->
            comp_vector(Path0, Line, Ctx0, Element);
        string ->
            comp_string(Path0, Line, Ctx0, Element)
    end.

%%=============================================================================
%% Internal Functions
%%=============================================================================
-spec comp_list(jxa_annot:path(), non_neg_integer(), jxa_ctx:context(), list()) ->
                       cerl:cerl().
comp_list(_Path0, _Line, _Ctx0, []) ->
    cerl:c_nil();
comp_list(Path0, Line, Ctx0, [H | T]) ->
    Path1 = jxa_annot:add_base_position(1, Path0),
    cerl:ann_c_cons([Line], comp(Path0, Ctx0, H), comp_list(Path1, Line, Ctx0, T)).

-spec comp_vector(jxa_annot:path(), non_neg_integer(), jxa_ctx:context(), list()) ->
                         cerl:cerl().
comp_vector(Path0, Line, Ctx0, Elements0) ->
    {_, Elements1} =
        lists:foldl(fun(Element, {Count0, Acc0}) ->
                            Count1 = Count0 + 1,
                            Acc1 = [comp(jxa_annot:add(Count1, Path0), Ctx0, Element) | Acc0],
                            {Count1, Acc1}
                    end, {0, []}, Elements0),
    cerl:ann_c_tuple([Line],
                     Elements1).

-spec comp_string(jxa_annot:path(), non_neg_integer(), jxa_ctx:context(), list()) ->
                         cerl:cerl().
comp_string(_Path0, Line, _Ctx0, String) ->
    cerl:ann_c_string([Line], String).
