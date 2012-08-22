---
layout: default
title: Joxa
version: 0.1.0
---

__Joxa is a small semantically clean, functional lisp__. It is a
general-purpose language encouraging interactive development and a
functional programming style. Joxa runs on the Erlang Virtual
Machine. Like other Lisps, Joxa treats code as data and has a full
(unhygienic) macro system.

Joxa (pronounced 'jocksah') isn't Erlang, though its very
compatible. Its not Clojure though there is plenty of shared
syntax. It's not Common Lisp though that is the source of the macro
system. While Joxa shares elements of many languages, it is its own
specific language.  of all these languages, and knowing these
languages will help you get up to speed with Joxa, but it is its own
unique language.

##### Functional Example - Sieve of Eratosthenes


    (ns sieve-of-eratosthenes
            (require lists io)
            (use (joxa-core :as core :only (!=/2))
                 (erlang :only (rem/2 +/2))))

    (defn sieve (v primes)
      (case v
        ([] primes)
        ((h . t)
          (sieve  (lists/filter (fn (x)
                                 (!= (rem x h) 0)) t)
                  (+ primes 1)))))

    (defn+ sieve (v)
      (sieve (lists/seq 2 v) 1))


##### Functional Example - Fibonacci


    (ns fibonacci
            (use (erlang :only (>/2 -/2 +/2))))

    (defn+ fibo (n)
      (case n
        (0 0)
        (1 1)
        (_ (when (> n 0))
         (+ (fibo (- n 1))
            (fibo (- n 2))))))


**NOTE**: At the moment Joxa is a an beta product. It is used in
production, and fully self hosting. However, you should expect to find
bugs and hopefully report and/or fix those bugs. There are also still
a number of things that need to be done as far as libraries and the
like. We encourage you to use Joxa and participate in the community.
however, you should expect to run into issues.


### Installation

You need to drop Joxa executable in your path, assuming you already
have erlang installed. That executable is an 'escript'. An escript is
basically a binary executable. However, it depends on the existence
(on your machine) of the Erlang Virtual Machine. So either install
that [now from source](http://www.erlang.org) or install it from the
packaging system on your distribution.

### Usage

Usage is quite simple. Run joxa without arguments to bring up the
shell.

    $> joxa
    Joxa {{ page.version }}

    joxa-shell> (defn foo () :hello)
    ok
    joxa-shell> (foo)
    hello
    joxa-shell>


To run the compile a joxa file simply run joxa with a -c and the file name.

    $> joxa -c my-ns.jxa
    $> ls
    my-ns.beam

### More Information

The Joxa [Docs](http://docs.joxa.org) are quite good and are kept up
to date. To explore further head over to the docs and start working
through the examples.
