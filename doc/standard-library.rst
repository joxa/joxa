Standard Library
****************

Core
====

!=
--


This is a 'not equal' operator for Joxa. It is basically equivelent to
(not (= ...)) or the erlang `=:=`

Example
^^^^^^^

.. code-block:: clojure

    joxa-is> (joxa-core/!= 1 2)
    :true

    joxa-is> (joxa-core/!= 1 1)
    :false


lte
---

Less then or equal to. Basically equivalent to `=<`. However this has
some issue in Joxa's syntax but `lte` solves these problems.

Example
^^^^^^^

.. code-block:: clojure

    joxa-is> (joxa-core/lte 1 2)
    :true

    joxa-is> (joxa-core/lte 1 1)
    :true

    joxa-is> (joxa-core/lte 10 1)
    :false

gte
---

Greater then or equal to. Basically equivalent to `>=`. However this has
some issue in Joxa's syntax but `gte` solves these problems.

Example
^^^^^^^

.. code-block:: clojure

    joxa-is> (joxa-core/gte 1 2)
    :false

    joxa-is> (joxa-core/gte 1 1)
    :true

    joxa-is> (joxa-core/gte 10 1)
    :true

and
---

This is a boolean `and` operation. The main difference between this
and `erlang/and/2` is that this allows an unlimited number of
arguments, in the tradition of lisp.

Example
^^^^^^^

.. code-block:: clojure

    joxa-is> (joxa-core/and :true :true :false)
    :false

    joxa-is> (joxa-core/and :true :true :true (joxa-core/!= 1 2))
    :true


or
--

This is a boolean `or` operation. The main difference between this
and `erlang/or/2` is that this allows an unlimited number of
arguments, in the tradition of lisp.

Example
^^^^^^^

.. code-block:: clojure

    joxa-is> (joxa-core/or :true :true :false)
    :true

    joxa-is> (joxa-core/or :true :true :true (joxa-core/!= 1 2))
    :true

    joxa-is> (joxa-core/or :false :false :false)
    :false



+
--

This is the multi-argument version of `erlang/+`. It does a simple
arithmetic addition for an unlimited number of arguments.

Example
^^^^^^^

.. code-block:: clojure

    joxa-is> (joxa-core/+ 1 2 3 4 5 6 7)
    28


-
-

This is the multi-argument version of `erlang/-`. It does a simple
arithmetic subtraction for an unlimited number of arguments.

Example
^^^^^^^

.. code-block:: clojure

    joxa-is> (joxa-core/+ 1 2 3 4 5 6 7)
    -26


incr
----

This increments a numeric value (either float or integer)

Example
^^^^^^^

.. code-block:: clojure

   joxa-is> (joxa-core/incr 1)
   2

   joxa-is> (joxa-core/incr 1.0)
   2.0


decr
----

This decrements a numeric value (either float or integer)

Example
^^^^^^^

.. code-block:: clojure

   joxa-is> (joxa-core/decr 1)
   0

   joxa-is> (joxa-core/incr 1.0)
   0.0


if
--

.. code-block:: none

    if test-form then-form else-form => result*

Arguments and Values
^^^^^^^^^^^^^^^^^^^^

test-form
  a form.

then-form
  a form.

else-form
  a form.

results
  if the test-form yielded true, the values returned by the then-form;
  otherwise, the values returned by the else-form.

Description
^^^^^^^^^^^

if allows the execution of a form to be dependent on a single
test-form.

First test-form is evaluated. If the result is true, then then-form is
selected; otherwise else-form is selected. Whichever form is selected
is then evaluated.

Examples
^^^^^^^^

.. code-block:: none

    joxa-is> (joxa-core/if :true 1 2)
    1
    joxa-is> (joxa-core/if :false 1 2)
    2


when
----

.. code-block:: none

    when test-form form* => result*

Arguments and Values
^^^^^^^^^^^^^^^^^^^^

test-form
  a form.

forms
  an implicit do.

results
  the values of the forms in a when form if the test-form yields `:true`
  or in an unless form if the test-form yields `:false`; otherwise `:ok`.

Description
^^^^^^^^^^^
when allows the execution of forms to be dependent on a single test-form.

In a when form, if the test-form yields true, the forms are evaluated
in order from left to right and the values returned by the forms are
returned from the when form. Otherwise, if the test-form yields false,
the forms are not evaluated, and the when form returns `:ok`.

Examples
^^^^^^^^

.. code-block:: none

    joxa-is> (joxa-core/when :true :hello)
    hello
    joxa-is> (joxa-core/when :false :hello)
    ok
    joxa-is> (joxa-core/when :true (io/format "1") (io/format "2") (io/format "3"))
    123


unless
------

.. code-block:: none

    unless test-form form* => result*

Arguments and Values
^^^^^^^^^^^^^^^^^^^^
test-form
  a form.

forms
  an implicit do.

results
  the values of the forms in a when form if the test-form yields `:true`
  or in an unless form if the test-form yields `:false`; otherwise `:ok`.

Description:

unless allows the execution of forms to be dependent on a single test-form.

In an unless form, if the test-form yields false, the forms are
evaluated in order from left to right and the values returned by the
forms are returned from the unless form. Otherwise, if the test-form
yields `:false`, the forms are not evaluated, and the unless form returns
`:ok`.

Examples
^^^^^^^^

.. code-block:: none

    joxa-is> (joxa-core/unless :true :hello)
    ok
    joxa-is> (joxa-core/unless :false :hello)
    hello
    joxa-is> (joxa-core/unless :true (io/format "1") (io/format "2") (io/format "3"))
    ok
    joxa-is> (joxa-core/unless :false  (io/format "1") (io/format "2") (io/format "3"))
    123


gensym
------

.. code-block:: none

    gensym => new-atom
    gensym x => new-atom

Arguments and Values
^^^^^^^^^^^^^^^^^^^^

x
  a string.

new-symbol
  a fresh, atom.

Description
^^^^^^^^^^^

Creates and returns a fresh, atom.

The name of the new-symbol is the concatenation of a prefix, which
defaults to "G", and a suffix, which is a randomly generated number.

If x is supplied, then that string is used as a prefix instead of "G"
for this call to gensym only.

Examples
^^^^^^^^

.. code-block:: clojure

   joxa-is> (joxa-core/gensym)
   '#:GAEECC9'
   joxa-is> (joxa-core/gensym "T")
   '#:|T66BA871|'

try
---

let-match
---------

define
------

.. code-block:: none

    define name value => form

Arguments and Values
^^^^^^^^^^^^^^^^^^^^
name
  an atom that represents the defined name

forms
  an arbitrary value

form
  a new defmacro that evaluates to the value

Description:

Defines a new macro that creates a compile time mapping between the
name and the value.


Examples
^^^^^^^^

.. code-block:: none

    joxa-is> (joxa-core/define :true :hello)
    ok
    joxa-is> (joxa-core/unless :false :hello)
    hello
    joxa-is> (joxa-core/unless :true (io/format "1") (io/format "2") (io/format "3"))
    ok
    joxa-is> (joxa-core/unless :false  (io/format "1") (io/format "2") (io/format "3"))
    123




Lists
=====

dolist

hd

tl

foldl

map

Records
=======

Records in Joxa are equivalent and compatible with records in
Erlang. However, like many things in Joxa they are used in very
different ways.

Before we get started there are a few things to keep in mind. Records
are designed to be contained in a single namespace. Defining multiple
records in the same namespace will cause the record system to stomp on
itself. That is the record macros generate many functions and macros
to access various parts of the record. With multiple records in the
same namespace those function names that are generated will conflict.

Overview
--------

Having a namespace per record is no big deal sense you can have
multiple namespaces per file. So to get started lets look at a
trivial, contrived example

.. code-block:: clojure

     (ns example-person
            (require erlang lists)
            (use (joxa-records :only (defrecord/2))))

     (joxa-records/defrecord+ person name age {sex male}
                              {address "unknown"} city)


     (ns example-walker
          (require example-person))

     (defn+ create-irish-walker ()
       (example-person/make
                          "Robert"
                          1024
                          :male
                          "Somewhere in Ireland"))

     (defn+ is-robert? (person)
       (case person
          ((example-person/t name "Robert")
             :true)
          (_
            :false)))

     (defn+ is-robert-male? (person)
       (case person
         ((example-person/t name "Robert"
                                 sex male)
            :true)
         (_
           :false)))

     (defn+ get-name-age-address (person)
        (example-person/let person
                           (name local-name
                            age local-age
                           address local-address)
          {local-name local-age local-address}))

This gives a quick overview of some of the things you can do with
records. Now lets jump into some detail.


Definition
----------

`joxa-records` is the namespace that contains the record system for
Joxa. The two functions that you will interact the most are
`defrecord` and `defrecord+`. `defrecord` and `defrecord+` both have
the exact same api. Just like with `defn` and `defmacro` the `+` added
to defrecord means that the record functions and macros will be
exported.

`defrecord` takes a name for the record followed by a list of field
descriptions. In our example we called our record `person`

.. code-block:: clojure

     (joxa-records/defrecord+ person name age {sex male}
                              {address "unknown"} city)

We then followed it up with the fields `name`, `age`, `sex`, `address`
and city. As you can see, the `sex` and `address` fields are a bit
different. That is because in these cases we are providing a default
value. So in record definitions you can provide just a name, or a name
and a default value. Just as a note, the name must be an atom and the
value a literal.

This is really all there is to it. The `defrecord` macro generates a
bunch of functions and macros used to interact with the record.

Creating Records
----------------

Once the record is defined we want to create it in the code that is
making use of the record. When a record is defined two functions are
defined that are used to create records. The first is the `make`
function. The second is the `make-fields` function.

Lets start by looking at the make function

.. code-block:: clojure

       (example-person/make
                        "Robert"
                        1024
                        :male
                        "Somewhere in Ireland"),

The make function is fairly strait forward, simple pass values for the
record in the order in which those fields where defined in the
`defrecord` definition. In this case you must pass a value for every
field in the record.

The make fields function is a bit more flexible. In this case you call
`make-fields` with a property list that provides a value for each
field that you are interested in defining a value for. Undefined
fields will simple get the value `undefined` or the default value
specified on record definition. As we can see in the following example

.. code-block:: clojure

       (example-person/make-fields
                       [{:name "Robert"}
                        {:age 1024}
                        {:address "Somewhere in Ireland"}])

We do not provide a value for the `sex` field or the `city` field. In
the case of `sex` the value will default to `mail` while in the case
of `city` it will default to `undefined`.

Getters and Setters
-------------------

`defrecord` generates several different ways of getting and setting
values from a function. The most strait forward of these is the field
name accessors. For each field defined Joxa generates a function to
get and set the value. The getter is a function with the name of the
field that takes a single value (the record). The setter is the name
of the field post-fixed by a `!` that takes the record as the first
argument as the new value as the second argument. So for example if we
wanted to get and set the `age` field of the person record we could do
the following

.. code-block:: clojure

     (let (age (example-person/age foo-record))
         (example-person/age! foo-record (+ age 1)))

`defrecord` also creates a set of anonymous getters and setters that
take the name of the field as an atom. These are the `element` and
`element!` functions. To accomplish the same thing we did above, but
with these anonymous functions we could do the following

.. code-block:: clojure

     (let (age (example-person/element :age foo-record))
         (example-person/element! foo-record :age (+ age 1)))

This makes it quite a bit easier to pragmatically manipulate a
record.

Finally, the record system provides a way for the use to get access
to several fields at the same time. This is accomplished through a
specialized let function. So lets say we wanted to get the `name`,
`age` and `address` fields from the record all at once. We could use
the generated let as follows

.. code-block:: clojure

        (example-person/let person-record
                           (name local-name
                            age local-age
                            address local-address)
          {local-name local-age local-address})

The first argument is the record that will have fields extracted. The
second argument is a list of field name, reference name pairs while
the rest is the body of the let. So in this case the value of the
`name` field in the `person-record` will be bound to the reference
`local-name` and be made available in the body of the let. The same is
true for `age` and `address`.


Pattern Matching
----------------

Joxa has pattern matching and, of course, you want to be able to
trivially match on records. To that end the Joxa record system
provides a macro that generated a matchable thing. That macro is the
`t` macro. The `t` macro takes a list of field name, data pairs that
are used to construct a pattern for that record. Lets look at some
examples. In the first example we want to create something that will
match on a record with the `name` "Robert" and nothing else

.. code-block:: clojure

    (case person-rec
     ((example-person/t name "Robert")
         :matched)
     (_
         :did-not-match))

If we want to match on more fields we can simple add more to the
field/value list

.. code-block:: clojure

    (case person
     ((example-person/t name "Robert"
                              sex male)
         :matched)
     (_
         :did-not-match))

or even

.. code-block:: clojure

    (case person
      ((example-person/t name "Robert"
                              sex :male
                              city :chicago)
          :matched)
      (_
          :did-not-match)))

Meta Data
---------

Finally the record system wants to give you the ability to do
unanticipated things when the need arises. So two functions are
defined to give you metadata data about the record. These functions
are `field-info/0` and `field-info/1`. Field info is a tuple of three
values that gives you the name of the field, the position of the field
in the tuple and its default value. In our example-person record the
result of `field-info/0` is

.. code-block:: clojure

  [{name,2,undefined},
   {age,3,undefined},
   {sex,4,male},
   {address,5,"Somewhere in Ireland"},
   {city,6,undefined}]

As you can see it gives you metadata for all the
fields. `field-info/1` returns the same metadata but only for a single
field. So if we called `field-info` with `name` we would get

.. code-block:: clojure

    {name,2,undefined}

Future Directions
-----------------

There is still a lot that can be added to records. Things like

* Pre and Post hook functions
* Types and automatic type validators

and more. However, the core defined here shouldn't change
significantly.

