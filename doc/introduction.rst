Introduction
************

Joxa is a Lisp designed to support general programming with good
declarative data description facilities. Joxa is intended to be used
as a powerful, light-weight alternative for Erlang for any program any
system where a language like Erlang is prefered. Joxa is implemented
as a compiler and library, written in itself while still making
extensive use of the Erlang libraries and infrastructure.

Joxa is free software, and is provided as usual with no guarantees, as
stated in its license. Further information is a available on the Joxa
website, www.joxa.org.

Examples
--------

The very first thing that everyone wants to see when exploring a new
language is what it looks like. So to feed that need lets jump right
into some examples and descriptions.


Sieve of Eratosthenes
~~~~~~~~~~~~~~~~~~~~~

Here we see the Sieve of Eratosthenes implemented as a Joxa Namespace::

    (ns sieve-of-eratosthenes
            (require lists)
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


Now that we have seen the entire namespace lets start breaking it
down::

    (ns sieve-of-eratosthenes
        (require lists)
            (use (joxa.core :as core :only (!=/2))
                (erlang :only (rem/2 +/2))))


The very first thing that must occur in file is the namespace special
form. You can call a fully qualified macro to create the namespace,
but that macro **must** create the namespace first. A namespace is
defined with the ns special form.

The ns special form consists of three main parts (we will go into
greater detail later in this document). The first part is the name of
the namespace. Which is an atom that identifies that namespace. Though is not
a requirement its generally a good idea for the namespace name  match the file
name.

The second part of the ns special form is the ``require`` form. The
require form provides a list of those namespaces that will be used by
the namespace. This is not strictly required (namespaces that are used
in the namespace being defined but not required will be automatically
required). However, it **is** very good documentation and I encourage
you to require all your namespaces.

The third part is the use form. The use form allow you to import
functions into the namespace. So you do not have to write the fully
qualified name. This is especially useful for functions and macros
defined as operators. Don't go crazy with it though. It is a spice
that should be used only where it enhances clarity.

Any number of require and use statements can appear in the namespace
in any order.

Next we see the function definition::

    (defn sieve (v primes)
      (case v
        ([] primes)
        ((h . t)
          (sieve  (lists/filter (fn (x)
                                 (!= (rem x h) 0)) t)
                  (+ primes 1)))))

We define a function called ``sieve`` that takes two arguments. The
argument ``v`` and, next, the argument ``primes``. We then have a
single ``case`` expression that forms the body of the function. A case
expression allows the author to do pattern matching on the second
clause of the expression. While he rest of the clauses identify
patterns and what will be evaluate based on the form of the output of
the second clause. In this example, you can see that an empty list
will return the argument ``primes`` unchanged while a cons cell will
result in a recursive call of ``sieve``, a call to the erlang module
``lists`` with an anonymous function. You can all see the use of the
functions (not defined in the namespace) that we imported into the
namespace with the use form.

Finally, we define our public api::

    (defn+ sieve (v)
      (sieve (lists/seq 2 v) 1))

There are two types of function definitions in Joxa; *exported* and
*unexported* functions. Exported functions are available outside of
the namespace while unexported functions are only available inside the
namespace itself. The difference in declaration is the use of
``defn+`` for exported functions in place of ``defn`` for unexported
functions. In this example you see us call the unexperted sieve
function and the use again of the lists erlang module. In Joxa, functions
must be defined before they are used. So the unexported ``sieve/2``
had to be defined before the exported ``sieve/1`` function.

Fibonacci
~~~~~~~~~

Here we see the Fibonacci implemented as a Joxa Namespace::

    (ns fibonacci
       (use (erlang :only (>/2 -/2 +/2))))

    (defn+ fibo (n)
      (case n
        (0 0)
        (1 1)
        (_ (when (> n 0))
         (+ (fibo (- n 1))
            (fibo (- n 2))))))
