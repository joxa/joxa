-module(jxat_concurrent_compiler).

-export([given/3, 'when'/3, then/3]).
-include_lib("eunit/include/eunit.hrl").

given([a,bunch,'of',joxa,source,files], _State, _) ->
    Files = [
        {
            "jxat-cc-foo-records.jxa",
            <<"(ns jxat-cc-foo-record-1
                 (use joxa-records))
               (defrecord+ a b c d)
               (ns jxat-cc-foo-record-2
                 (use joxa-records))
               (defrecord+ a b c d)
               (ns jxat-cc-foo-record-3
                 (use joxa-records))
               (defrecord+ a b c d)
               (ns jxat-cc-foo-record-4
                 (use joxa-records))
               (defrecord+ a b c d)
               (ns jxat-cc-foo-record-5
                 (use joxa-records))
               (defrecord+ a b c d)">>
       },
       {
           "jxat-cc-abc.jxa",
           <<"(ns jxat-cc-a
                (use joxa-records))
              (defrecord+ a b c)
              (ns jxat-cc-b
                (require jxat-cc-a))
              (defn+ foo () (jxat-cc-a/make 1 2 3))
              (ns jxat-cc-c)
              (defn+ bar () 42)">>
       },
       {
           "jxat-cc-foo.jxa",
           <<"(ns jxat-cc-foo
                (require jxat-cc-foo-record-1 (erlang :as erl)))
              (defn+ foo-fields ()
                (jxat-cc-foo-record-1/make-fields
                  [{:a 1} {:b (erl/time)} {:c 3} {:d 4}]))">>
       },
       {
           "jxat-cc-bar.jxa",
           <<"(ns jxat-cc-bar
                (require erlang jxat-cc-foo))
              (defn+ b () (erlang/element 3 (jxat-cc-foo/foo-fields)))">>
       },
       {
           "jxat-cc-baz.jxa",
           <<"(ns jxat-cc-baz
                (require
                  (erlang :as erl)
                  jxat-cc-foo-record-3
                  jxat-cc-foo-record-4))
              (defn+ a ()
                (erl/element 2 (jxat-cc-foo-record-3/make 1 2 3 4)))
              (defn+ b ()
                (erl/element 3 (jxat-cc-foo-record-4/make 1 2 3 4)))">>
        },
        {
            "jxat-cc-qux.jxa",
            <<"(ns jxat-cc-qux
                 (require io)
                 (use (erlang :only (now/0))))
               (defn+ print-time-now ()
                 (io/format \"~p~n\" [(now)]))">>
        }
    ],
    lists:foreach(fun({Filename, Source}) ->
                      file:write_file(".eunit/" ++ Filename, Source)
                  end, Files),
    {ok, [".eunit/" ++ Filename || {Filename, _Source} <- Files]}.

'when'([the,concurrent,compiler,is,called,with,these,files], State, _) ->
    {ok, 'joxa-concurrent-compiler':'do-compile'(State, [{outdir, ".eunit"}])}.

then([all,files,are,compiled,successfully], Result, _) ->
    ?assertMatch(ok, Result),
    Filenames = [
        ".eunit/jxat-cc-foo-record-1.beam",
        ".eunit/jxat-cc-foo-record-2.beam",
        ".eunit/jxat-cc-foo-record-3.beam",
        ".eunit/jxat-cc-foo-record-4.beam",
        ".eunit/jxat-cc-foo-record-5.beam",
        ".eunit/jxat-cc-a.beam",
        ".eunit/jxat-cc-b.beam",
        ".eunit/jxat-cc-c.beam",
        ".eunit/jxat-cc-foo.beam",
        ".eunit/jxat-cc-bar.beam",
        ".eunit/jxat-cc-baz.beam",
        ".eunit/jxat-cc-qux.beam"
    ],
    lists:foreach(fun(Filename) ->
                      ?assertMatch([_|_], beam_lib:info(Filename))
                  end, Filenames),
    {ok, Result}.
