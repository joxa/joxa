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

### A Quick Overview

    ;; Numbers
    1
    2.2
    2e3

    ;; Arrays/Tuples
    {1 2 3 4}

    ;; Atoms
    :this-is-an-atom
    'so-is-this
    :'this is too'

    ;; Binaries
    <<(1 :integer) (rest :binary)>>

    ;; Conditions
    (if (is-number? 1)
        :woo-hoo
      :so-sad)

    (when (is-number? 1)
       :woo-hoo)

    (unless (is-number? 1)
       :so-sad)

    (case (get-list)
      ([] :empty)
      ([1] :got-a-one)
      (else {:got-something-else}))

    ;; Functions
    (fn (arg1 arg2) (io/format "~p:~p~n" [arg1 arg2]))

    ;; Macros
    (defmacro alternate-when (arg &rest body)
       `(case ~arg
          (:true ~@body)
          (:false :ok)))

     ;; Public Functions
     (defn+ notice-the-plus () :woot!)

     ;; Private Functions
     (defn no-plus () :you-cant-see-me)


##### Functional Example - Sieve of Eratosthenes


    (module sieve-of-eratosthenes
            (require lists io)
            (use (joxa.core :as core :only (!=/2))
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


    (module fibonacci
            (use (erlang :only (>/2 -/2 +/2))))

    (defn+ fibo (n)
      (case n
        (0 0)
        (1 1)
        (_ (when (> n 0))
         (+ (fibo (- n 1))
            (fibo (- n 2))))))


**NOTE**: At the moment Joxa is a an Alpha product. It is used in
production, and fully self hosting. However, you should expect to find
bugs and hopefully report and/or fix those bugs. There are also still
a number of things that need to be done as far as libraries and the
like. We encourage you to use Joxa and participate in the community.
however, you should expect to run into issues.


### Installation

If you are using Ubuntu its easiest to install Joxa from the
[PPA](https://launchpad.net/~afiniate/+archive/ppa). This will get
everything setup for you.  For other distributions you simply need to
drop the Joxa executable in your path. That executable is an
'escript'. An escript is basically a binary executable. However, it
depends on the existence (on your machine) of the Erlang Virtual
Machine. So either install that
[now from source](http://www.erlang.org) or install it from the
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

    $> joxa -c my-mod.jxa
    $> ls
    my-mod.beam

### Language Reference

#### Tokens


    ;; Function Reference
    fun-name/3
    module-name/fun-name/3

    ;; Atoms
    atom, foo-bar, *foo*, :foo

    ;; Literals
    42
    "foo"
    :nil
    :true
    :false
    \c
    :foo

    ;; Lists
    '(a b c)
    [a b c]

    ;; Tuples
    {a b c}

    ;; Quote
    'form
    '(1 2 3)

    ;; Character
    \a
    \b
    \c

    ;; Comments
    ; txt

    ;; Syntax-quote
    ` (backtick)

    ;;Unquote
    ~(tilde)

    ;;Unquote-splicing
    ~@(tilde at)

    ;; Anonymous functions
    (fn [args] :woot)


#### Forms

##### Modules


    (module <module_name>
       <require>*
        <use>*
        <attributes>*)

     <require> :: (require <req-spec>+)
     <req-spec> :: <module-name> |
                   (<module-name> :as <alias>) |

     <attributes> :: (attr <ident> <literal>)

     <use> :: (use <use-spec>+)
     <use-spec> :: <module-name> |
                   <module-specializer>
     <module-specializer> :: (<module-name> <spec-pair>+)
     <spec-pairs> :: :only (<function ref>+) |
                     :exclude (<function ref>+) |
                     :rename (<rename-spec>+)
     <rename-spec> :: (<function-ref> <alias>)


###### Requires Examples

     (require erlang string test)
     (require string (test :as test))
     (require (string :as string))


###### Uses Examples

    (use string)
    (use (string :only (tokens/2)))
    (use (string :exclude (substr/3 join/2 join/3)))
    (use (string :rename ((substr/3 str-substring) (join/2 str-join))))
    (use (string :as str :only (join/2 substr/3)))
    (use (string :as str :only (tokens/2)))
    (use (string :as str :exclude (substr/3 join/2 join/3)))
    (use (string :as str :rename ((substr/3 str-substring) (join/2 str-join))))

###### Attributes

    (attr key 123)

##### Function Definitions

###### Exported Function


    (defn+ <name> (<argument>* <rest-argument>?)
        <expressions>+)
     <rest-argument> :: &rest <arg-name>


###### Non Exported Function


    (defn <name> (<argument>* <rest-argument>?)
        <expressions>+)
    <rest-argument> :: &rest <reference-name>


###### Inlined Function

A function declared this way will always be inlined, but will not be
exported


    (definline <name> (<argument>* <rest-argument>?)
        <expressions>+)
    <rest-argument> :: &rest <reference-name>


###### Exported Macro


    (defmacro+ <name> (<argument>* <rest-argument>?)
        <expressions>+)
    <rest-argument> :: &rest <reference-name>


###### Non Exported Macro


    (defmacro <name> (<argument>* <rest-argument>?)
        <expressions>+)
    <rest-argument> :: &rest <reference-name>


###### Type Definitions


     (deftype <name> (<arguments>*) <return-value>)
     (defspec <name> (<arguments>*) <return-value>)


#### Expressions

##### Literals

###### Atoms


     :some-atom
     'some-atom
     :'some atom with things like \' and : and ( [ etc in it'


###### Integer


     <normal integers>
     1234
     -12


###### Float


     <normal float>
     12.33
     -1.0
     1+e8


###### Tuple


     {a1 a2 a3 a4}
     (tuple a1 a2 a3 a4)


###### List


     '(a1 a2 a3 a4 ...)
      [a1 a2 a3 a4 ...]
      (list a1 a2 a3 a4)


##### Binary



    << (<bitstrings>+ >>
    (binary <bitstrings>+ )

    <bitstrings> ::  (<reference-name> <size-spec>? <unit-spec>? <endianness>? <signedness>? <type>?)
     <size-spec> :: (:size <integer>)
     <unit-spec> :: (:unit <integer>)
     <endianness> :: :big | :little | :native
     <signedness> :: :unsigned | :signed
     <type> :: :float | :integer | :bitstring | :bits | :binary | :utf8 | :utf16 | :utf32



###### Function Calls


     (<function-name> <argument>*)


###### Let


    (let (<references>*) <expressions>+)
    <references> :: <reference-name> <expression>


###### Case


     (case <expression> <clauses>+)
      <clauses> :: (<pattern> <guard>? <expressions>+)
      <guard> :: (when <expression>)


You may only have one guard expression. Treat this like an if
statement, so if you need more then one you must use an `erlang/or` or
`erlang/and` etc.

###### Anonymous Fun


    (fn (<argument>*) <expression>+)


###### Apply

`
    (apply <fun-reference> <arg>*)
    <fun-reference> :: <module-name>/<function-name>/<arity> |
                       <function-name>/<arity> |
                       <reference-name>


###### Primitive Try


    (--try <expression> <catch-clause>)
    <catch-clause> :: (catch (<reference> <reference>) <expression>+)


The expr is evaluated and, if no exceptions occur, the value of the
last is returned. If an exception occurs and catch clauses are
provided, each is matched in turn and the first matching exception is
called. If there is a matching catch clause, its expr are evaluated
in a context in which name is bound to the thrown exception, and the
value of the last is the return value of the function. If there is no
matching catch clause, the exception propagates out of the function.

###### Core Try

There is another try implemented as a macro in core. This is the try
that it is expected that the vast majority of users will use. It
provides a much more user friendly format then the primitive try.


    (try <expression> <catch-clause>)
    <catch-clause> :: (catch <clause>+)
    <clause> :: ({<type-pattern> <error-pattern>} <expression>)


###### Receive


    (receive <after-clause>? <clause>+)
    <after-clause> :: (after <timeout> <expression>+)
    <timeout> :: <integer>
    <clause> :: (<pattern> <expression>+)


###### Do


    (do
      <expression>+)
