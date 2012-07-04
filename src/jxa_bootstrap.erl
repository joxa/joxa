%% THIS IS A GENERATED FILE, DO NOT EDIT DIRECTLY
-module(jxa_bootstrap).

-export([do_bootstrap/1]).

do_bootstrap([OutputFile, InputFile]) ->
    {ok, [AST]} = file:consult(InputFile),
    io:format("writing beam ~p~n", [OutputFile]),
    case compile:forms(AST, [binary, from_core, return_errors, debug_info]) of
        {ok , _, Result} ->
            ok = file:write_file(OutputFile, Result);
        Error ->
            erlang:throw(Error)
    end.
