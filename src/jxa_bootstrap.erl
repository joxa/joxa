%% THIS IS A GENERATED FILE, DO NOT EDIT DIRECTLY
-module(jxa_bootstrap).

-export([do_bootstrap/1]).

do_bootstrap([OutputDir, InputFile]) ->
    {ok, [AST]} = file:consult(InputFile),
    {c_module, _,
     {c_literal, _, ModuleName}, _, _, _} = AST,
    io:format("writing beam to dir~p~n", [OutputDir]),
    case compile:forms(AST, [binary, from_core, return_errors, debug_info]) of
        {ok, _, Result} ->
            io:format("Module Name ~p~n", [ModuleName]),
            OutputFile = filename:join(OutputDir,
                                      erlang:atom_to_list(ModuleName) ++ ".beam"),
            ok = file:write_file(OutputFile, Result);
        Error ->
            erlang:throw(Error)
    end.
