#!/usr/bin/env escript
%%! -pa ebin

main(["ast", SrcDir, ASTDir]) ->
    %% compiling joxa to ast files
    update_code_path(),
    io:format("~n--- making ast files ---~n"),
    F = fun(E) ->
                JxaFile = to_jxa(SrcDir, E),
                ASTFile = to_ast(ASTDir, E),
                case should_skip(JxaFile, ASTFile) of
                    false ->
                        %% compile JxaFile to AST if it's not compiled already
                        io:format("writing ast to dir ~p~n", [ASTDir]),
                        'joxa-compiler':'do-compile'(JxaFile,
                                                     [to_ast, {outdir, ASTDir}]),
                        file:write_file(ASTFile, ".", [append]),
                        io:format("File '~s'~n", [ASTFile]);
                    true ->
                        ok
                end
        end,
    lists:foreach(F, modules());
main(["bootstrap", ASTDir, OutDir]) ->
    %% bootstrapping joxa
    update_code_path(),
    io:format("~n--- bootstrapping ---~n"),
    F = fun(E) ->
                File = to_ast(ASTDir, E),
                case should_skip(File, to_beam(OutDir, E)) of
                    false ->
                        %% compile File if it is not compiled already
                        jxa_bootstrap:do_bootstrap([OutDir, File]);
                   true ->
                        ok
                end
        end,
    lists:foreach(F, modules());
main(["compile", SrcDir, OutDir]) ->
    %% compiling the rest of joxa (after botstrapping)
    update_code_path(),
    io:format("~n--- compiling ---~n"),
    F = fun(E) ->
                File = to_jxa(SrcDir, E),
                case should_skip(File, to_beam(OutDir, E)) of
                    false ->
                        %% compile File if it is not compiled already
                        io:format("writing beam to dir ~p~n", [OutDir]),
                        'joxa-compiler':'do-compile'(File, [{outdir, OutDir}]),
                        io:format("Module Name '~s'~n", [E]);
                   true ->
                        ok
                end
        end,
    lists:foreach(F, src_beams()).

%% modules to be compiled for bootstrapping joxa
modules() ->
    ["joxa-cmp-util",
     "joxa-cmp-path",
     "joxa-cmp-ctx",
     "joxa-cmp-peg",
     "joxa-cmp-lexer",
     "joxa-cmp-ns",
     "joxa-cmp-call",
     "joxa-cmp-literal",
     "joxa-cmp-binary",
     "joxa-cmp-special-forms",
     "joxa-cmp-case",
     "joxa-cmp-spec",
     "joxa-cmp-expr",
     "joxa-cmp-defs",
     "joxa-cmp-joxa-info",
     "joxa-cmp-checks",
     "joxa-cmp-error-format",
     "joxa-cmp-parser",
     "joxa-compiler"].

%% the rest of modules to be compiled
src_beams() ->
    ["joxa-core",
     "joxa-shell",
     "joxa-records",
     "joxa-assert",
     "joxa-eunit",
     "joxa-lists",
     "joxa-otp",
     "joxa-otp-gen-server",
     "joxa-sort-topo",
     "joxa-concurrent-compiler",
     "joxa-cc-wkr",
     "joxa",
     "joxa-build-support",
     "joxa-otp-application",
     "joxa-otp-supervisor"].

to_jxa(SrcDir, Module) ->
    %% SrcDir/Module.jxa
    filename:join(SrcDir, Module ++ ".jxa").

to_ast(ASTDir, Module) ->
    %% ASTDir/Module.ast
    filename:join(ASTDir, Module ++ ".ast").

to_beam(EbinDir, Module) ->
    %% EbinDir/Module.beam
    filename:join(EbinDir, Module ++ ".beam").

update_code_path() ->
    %% add deps/*/ebin to code path
    code:add_pathsz(filelib:wildcard(filename:join(["deps", "*", "ebin"]))).

last_modified(File) ->
    case filelib:last_modified(File) of
        0 ->
            0;
        Else ->
            calendar:datetime_to_gregorian_seconds(Else)
    end.

should_skip(Source, Destination) ->
    last_modified(Source) < last_modified(Destination).
