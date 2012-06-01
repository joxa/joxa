The Joxa Language
=================

Type Specs
----------

Mutually Recursive Modules
~~~~~~~~~~~~~~~~~~~~~~~~~

In Joxa code must exist at compile time before it is called. That
means that if you are compiling a module and it calls other modules
those other modules must exist to be called (at compile time). If they
are not it is a build failure. Unfortunately, this makes mutually
recursive functions somewhat difficult. In general mutually recursive
modules are something to be avoided. However, at times they are needed
and there is no way to get around that need. When this occurs Joxa
provides a facility to get around it. This is very similar to its
forward declarations via defspecs. That way is to define a spec for
the remote function. Lets take a look at an example of this::

    (ns joxa.exp.nmr-ns1)

    (defn+ final ()
       :got-it)

    ;; Forward declaration for ns2
    (defspec joxa.exp.nmr-ns2/recurse-ns1 () (erlang/any))

    (defn+ recurse-ns2 ()
      (joxa.test.nmr-ns2/recurse-ns1))

    ;; ======

    (ns joxa.exp.nmr-ns2)

    (defspec joxa.exp.nmr-ns1/final () (erlang/any))

    (defn+ recurse-ns1 ()
       (joxa.exp.nmr-ns1/final))

Notice that joxa.exp.nmr-ns1 has a dependency on joxa.exp.nmr-ns2 and
vice versa. In normal Joxa code this would not be compilable because
the code that is being called must be available before it is
called. However, we have gotten around this problem by providing
remote defspecs. In joxa.exp.nmr-ns1 we pre-declare
joxa.exp.nmr-ns2/recurse-ns1 while in joxa.exp.nmr-ns2 we pre-declare
joxa.exp.nmr-ns1/final. This allows the Joxa compiler to check the
function against the specs instead of the real module. Of course,
there is no way for the compiler to know if those functions actually
exist, so if you make a mistake you may actually get runtime
errors. So be careful.
