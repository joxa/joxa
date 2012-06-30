The Joxa Language
*****************

Special Forms
-------------

let*
^^^^

.. code-block:: clojure

  (let* (val val-expr ...) expr ...)


try*
^^^^

.. code-block:: clojure

    (try* expr (catch (error-class error-type) catch-expr ...))


case
^^^^

.. code-block:: clojure

   (case expr (pattern <optional-guard> expr ...) ...)



receive
^^^^^^^

.. code-block:: clojure

    (receive <optional-after> (pattern <optional-guard> expr ...) ...)


do
^^

.. code-block:: clojure

   (do expr ...)


binary
^^^^^^

$filename
^^^^^^^^^

.. code-block:: clojure

    ($filename)


$namespace
^^^^^^^^^^

.. code-block:: clojure

    ($namespace)


$line-number
^^^^^^^^^^^^

.. code-block:: clojure

    ($line-number)


$function-name
^^^^^^^^^^^^^^

.. code-block:: clojure

    ($function-name)


apply
^^^^^

.. code-block:: clojure

    (apply fun [args ...])


quote
^^^^^

.. code-block:: clojure

    (quote expr ...)
    'expr
    :atom

quasiquote
^^^^^^^^^^

.. code-block:: clojure

   `expr


string
^^^^^^

.. code-block:: clojure

   (string "values")


list
^^^^

.. code-block:: clojure

   (list expr ...)
   [expr ...]

tuple
^^^^^

.. code-block:: clojure

   (tuple expr ...)
   {expr ...}

macroexpand-1
^^^^^^^^^^^^^

.. code-block:: clojure

   (macroexpand-1 expr ...)


fn
^^^

.. code-block:: clojure

   (fn (arg ...) expr ...)


Namespaces
----------

`ns` declarations are used to define the namespace in which a set of
definitions live. The generally also define the context, that is what
other namespaces are available, what functions from other namespaces
are imported and what attributes are defined. A basic namespace
declaration looks as follows.

.. code-block:: clojure

    (ns my-super-module)

This defines a namespace the `defn` and `defmacro` definitions that
follow are part of that namespace. The namespace must be defined
before the functions using that namespace. You may also have as many
namespaces as you would like per file, though that is not encouraged.

Namespace Body
""""""""""""""

The namespace body may consist of any number of `require`, `use` and
clauses in any order and in any conversation.

Requiring Namespaces
^^^^^^^^^^^^^^^^^^^^

Other namespaces are *not* available in your namespace until you
declare your need in a `require` or `use` clause. For example the
following namespace will fail during compile.

.. code-block:: clojure

    (ns my-converter)

    (defn+ convert-string (str)
        (erlang/binary_to_list str))

This would fail during compilation because you have not declared your
that you are going to use the erlang namespace. We can fix this by
adding a require clause.

.. code-block:: clojure

    (ns my-converter
       (require erlang))

    (defn+ convert-string (str)
        (erlang/binary_to_list str))

Suddenly everything compiles happily.

There are several variations to the require clause that you can
use. The variation you use is really up to you. For example to require
multiple namespaces you could have them all in the same require clause
or each on individual require clauses.


.. code-block:: clojure

    (require erlang string test)

    (require erlang)
    (require string)
    (require test)

in general it is much more common to include everything in a single
require clause.

Aliasing with Require
"""""""""""""""""""""

Sometimes namespaces names are very long and its annoying to use them
in the namespace body. To avoid this you can add an `:as` element to
the require clause. This allows you to use both the original name and
the aliased name in your namespace body. For example, if we use
erl_prim_loader we might want to rename it as loader.

.. code-block:: clojure

     (ns my-example
        (require (erl_prim_loader :as loader)))

     (defn name-example ()
        (erl_prim_loader/get_path))

     (defn alias-example ()
        (loader/get_path))

Both of these examples are functionally equivalent.

Making Erlang Modules Appear Like Joxa Namespaces (Joxification)
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

Its much more common in Joxa to use the `-` in names as opposed to the
`_` as is common in Erlang. To make thing more comfortable for the
namespace definer Joxa offers the `joxify` element for require
clauses. the `joxify` element basically aliases defined names from a
name containing `_` to a name containing `-`. It also does this for
all the functions in the module.

Lets use our `erl_prim_loader` example again.

.. code-block:: clojure

     (ns my-example
        (require (erl_prim_loader :joxify)))

     (defn name-example ()
        (erl_prim_loader/get_path))

     (defn alias-example ()
        (erl-prim-loader/get-path))

Again both of these are functionally Equivalent.

Attribute Clauses
^^^^^^^^^^^^^^^^^

Attribute clauses are the simplest of the three clauses There are
simply a three element list where the first element is the identifier
'attr', the second element is a Joxa term that provides the key value
and the third is a Joxa term that provides the value.

Attributes follow the form:

.. code-block:: clojure

     (attr <key> <value>)

These allow you to define attributes on the namespace. Some of which
are consumable by the compiler, others just informational, all though
are consumable via the module_info. You should note that both the key
and the value must be literal values, no evaluation occurs there.

Using Namespaces
^^^^^^^^^^^^^^^^

The use clause is a way of importing functions into the namespace so
that you can use them without prepending the namespace. Use clauses
are, by far, the most complex of the namespace clauses as they both
manipulate and subset the functions being imported while at the same
time aliasing the function if desired. As you can see below each
clause may consist of a namespace name, or a list that contains a few
subclauses.  The sub-clause is always headed by a namespace name,
followed by an action, followed by the subject of that action. The
action/subject may be repeated to further refine and modify the
imported values. The sub-clause action/subject may occure in any
order. Even though some do not make sense when used together. So, for
example you could have the following

.. code-block:: clojure

     (use string)

     (use (string :only (tokens/2)))

     (use (string :exclude (substr/3
                            join/2
                            join/3)))

     (use (string :rename ((substr/3 str-substring)
                           (join/2 str-join))))

     (use (string :as str
                  :only (join/2
                         substr/3)))

     (use (string :as str
                  :only (tokens/2)))

     (use (string :as str
                  :exclude (substr/3
                            join/2
                            join/3)))

     (use (string :as str
                  :joxify
                  :rename ((substr/3 str-substring)
                           (join/2 str-join))))

You should think about use clauses as a series of actions that occur
from left to right. Lets take an example and work through it. The
following is a fairly complex example that highlights some things that
we might want to do.

.. code-block:: clojure

     (use (string :exclude (substr/4 join/2)
                  :joxify
                  :rename ((sub-word/3 str-subword) (join/2 str-join))))

Lets break this down into actions.

1) The namespace declaration. In this case `string`, this goes to the
   namespace and gets a list of all the functions that that namespace
   exports. That list of functions is then passed to the next 'operation'.
2) Exclude, this excludes the specified functions from the function
   list that was imported. Every action/sub-clause pair after this
   exclude will only operate on the functions that have not been
   excluded. The opposite of exclude is `only`. Only subsets the list
   of functions to just those specified in the only clause.
3) Joxify, This does the exact same thing that joxify does in
   require. However, it does it only on the module name and the
   functions that we currently have in the list. After this point the
   functions in the list can only be referred to by the joxified name.
4) Rename. This does what you would think. It renames a function
   giving it a different name. This does this on the list of functions
   being passed forward. In this example we are renaming `sub-word/3`
   to `str-subword`. However if we tried to rename `substr/4` which we
   excluded it would have no effect since its not in the list of
   imports being carried forward. *NOTE* note the joxification of
   `sub-word/3`. Since we specified `joxify` earlier we must must
   refer to it as `sub-word/3` instead of `sub_word/3`.

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
