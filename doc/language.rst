The Joxa Language
*****************

Functions
---------

&rest Arguments to Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Rest arguments in a language like Joxa, where arity is basically part
of the namespace, take a bit of thought to get your mind
around. Basically, Joxa like Lisp has the ability to group all
remaining arguments into a list at the discretion of the function
implementer. This changes the way those functions are called and
perhaps referred to.

Defined Functions
"""""""""""""""""

In module defined functions rest arguments work like you would
expect. For example:

.. code-block:: clojure

    (defn+ i-am-a-rest-fun (arg1 arg2 &rest arg3)
        {arg1 arg2 arg3})

In this case, any time `i-am-a-rest-fun` is called, the arguments are
collapsed down for the third argument. This happens for any call that
has more then three arguments.

In this case of namespaces `i-am-a-rest-fun/3` can actually be
referred to by any arity that is 3 or greater. For example
`i-am-a-rest-fun/545` still refers to `i-am-a-rest-fun/3` because
those extra arguments are simply collapsed to the three. With that in
mind you could define `i-am-a-rest-fun/2` without a problem. However,
you could never define `i-am-a-rest-fun/5` because `i-am-a-rest-fun/3`
overrides anything with arguments three or greater. to give a concrete
example, you could define:

.. code-block:: clojure

    (defn+ i-am-a-rest-fun (arg1 arg2)
        {arg1 arg2})

and it would be valid and make sense. However, you could not define

.. code-block:: clojure

    (defn+ i-am-a-rest-fun (arg1 arg2 arg3 arg4)
        {arg1 arg2 arg3 arg4})

Because `i-am-a-rest-fun/3` already fills that namespace completely.

Anonymous Functions
"""""""""""""""""""

Anonymous functions work exactly like defined functions. I could do

.. code-block:: clojure

    (fn (one two &rest three)
       {one two three})

I can then assign that to the variable `foo` and call `foo` as:

.. code-block:: clojure

    (foo 1 2 3 4 5 6 7 8 9)

and it would do the correct thing.

Variables that Refer to Functions
"""""""""""""""""""""""""""""""""

For the most part variables that reference rest functions work exactly
like you would expect. However, in the case where the 'restful-ness'
of a variable can not be defined at compile time, a function is
created that does the resolution at run time. This mostly happens when
variables are passed as arguments to functions. At the moment the
argument boundary can not be crossed, so when those variables are used
as functions, they are wrapped in a function that does the runtime
resolution and calls the correct function with the correct args. This
may affect performance.

Apply
"""""

Apply also works exactly as you would expect. Any resolvable rest call
has the arguments handled correctly at compile time. Any un-resolvable
rest call has a function created to correctly handle the arguments at
runtime.

Importing Rest Functions via Use
""""""""""""""""""""""""""""""""

The `use` clause in module declarations take a bit of thinking. To
refer to a function in a use clause use the actual arity. In our
function above you would use `(use :only i-am-a-rest-fun/3)`

Type Specs
----------

Mutually Recursive Modules
^^^^^^^^^^^^^^^^^^^^^^^^^^

In Joxa code must exist at compile time before it is called. That
means that if you are compiling a module and it calls other modules
those other modules must exist to be called (at compile time). If they
are not it is a build failure. Unfortunately, this makes mutually
recursive functions somewhat difficult. In general mutually recursive
modules are something to be avoided. However, at times they are needed
and there is no way to get around that need. When this occurs Joxa
provides a facility to get around it. This is very similar to its
forward declarations via defspecs. That way is to define a spec for
the remote function. Lets take a look at an example of this

.. code-block:: clojure

    (ns Joxa-exp-nmr-ns1)

    (defn+ final ()
       :got-it)

    ;; Forward declaration for ns2
    (defspec Joxa-exp-nmr-ns2/recurse-ns1 () (erlang/any))

    (defn+ recurse-ns2 ()
      (joxa-test-nmr-ns2/recurse-ns1))

    ;; ======

    (ns joxa-exp-nmr-ns2)

    (defspec joxa-exp-nmr-ns1/final () (erlang/any))

    (defn+ recurse-ns1 ()
       (joxa-exp-nmr-ns1/final))

Notice that `joxa-exp-nmr-ns1` has a dependency on `joxa-exp-nmr-ns2`
and vice versa. In normal Joxa code this would not be compilable
because the code that is being called must be available before it is
called. However, we have gotten around this problem by providing
remote defspecs. In `joxa-exp-nmr-ns1` we pre-declare
`joxa-exp-nmr-ns2/recurse-ns1` while in `joxa-exp-nmr-ns2` we
pre-declare `joxa-exp-nmr-ns1/final`. This allows the Joxa compiler to
check the function against the specs instead of the real module. Of
course, there is no way for the compiler to know if those functions
actually exist, so if you make a mistake you may actually get runtime
errors. So be careful.
