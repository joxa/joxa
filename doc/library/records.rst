Records
-------

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
~~~~~~~~

Having a namespace per record is no big deal sense you can have
multiple namespaces per file. So to get started lets look at a
trivial, contrived example::


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
~~~~~~~~~~

`joxa-records` is the namespace that contains the record system for
Joxa. The two functions that you will interact the most are
`defrecord` and `defrecord+`. `defrecord` and `defrecord+` both have
the exact same api. Just like with `defn` and `defmacro` the `+` added
to defrecord means that the record functions and macros will be
exported.

`defrecord` takes a name for the record followed by a list of field
descriptions. In our example we called our record `person`::

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
~~~~~~~~~~~~~~~~

Once the record is defined we want to create it in the code that is
making use of the record. When a record is defined two functions are
defined that are used to create records. The first is the `make`
function. The second is the `make-fields` function.

Lets start by looking at the make function::

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
specified on record definition. As we can see in the following example::

       (example-person/make-fields
                       [{:name "Robert"}
                        {:age 1024}
                        {:address "Somewhere in Ireland"}])

We do not provide a value for the `sex` field or the `city` field. In
the case of `sex` the value will default to `mail` while in the case
of `city` it will default to `undefined`.

Getters and Setters
~~~~~~~~~~~~~~~~~~~

`defrecord` generates several different ways of getting and setting
values from a function. The most strait forward of these is the field
name accessors. For each field defined Joxa generates a function to
get and set the value. The getter is a function with the name of the
field that takes a single value (the record). The setter is the name
of the field post-fixed by a `!` that takes the record as the first
argument as the new value as the second argument. So for example if we
wanted to get and set the `age` field of the person record we could do
the following::

     (let (age (example-person/age foo-record))
         (example-person/age! foo-record (+ age 1)))

`defrecord` also creates a set of anonymous getters and setters that
take the name of the field as an atom. These are the `element` and
`element!` functions. To accomplish the same thing we did above, but
with these anonymous functions we could do the following::

     (let (age (example-person/element :age foo-record))
         (example-person/element! foo-record :age (+ age 1)))

This makes it quite a bit easier to pragmatically manipulate a
record.

Finally, the record system provides a way for the use to get access
to several fields at the same time. This is accomplished through a
specialized let function. So lets say we wanted to get the `name`,
`age` and `address` fields from the record all at once. We could use
the generated let as follows::

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
~~~~~~~~~~~~~~~~

Joxa has pattern matching and, of course, you want to be able to
trivially match on records. To that end the Joxa record system
provides a macro that generated a matchable thing. That macro is the
`t` macro. The `t` macro takes a list of field name, data pairs that
are used to construct a pattern for that record. Lets look at some
examples. In the first example we want to create something that will
match on a record with the `name` "Robert" and nothing else::


    (case person-rec
     ((example-person/t name "Robert")
         :matched)
     (_
         :did-not-match))

If we want to match on more fields we can simple add more to the
field/value list::

    (case person
     ((example-person/t name "Robert"
                              sex male)
         :matched)
     (_
         :did-not-match))

or even::

    (case person
      ((example-person/t name "Robert"
                              sex :male
                              city :chicago)
          :matched)
      (_
          :did-not-match)))

Meta Data
~~~~~~~~~

Finally the record system wants to give you the ability to do
unanticipated things when the need arises. So two functions are
defined to give you metadata data about the record. These functions
are `field-info/0` and `field-info/1`. Field info is a tuple of three
values that gives you the name of the field, the position of the field
in the tuple and its default value. In our example-person record the
result of `field-info/0` is::

  [{name,2,undefined},
   {age,3,undefined},
   {sex,4,male},
   {address,5,"Somewhere in Ireland"},
   {city,6,undefined}]

As you can see it gives you metadata for all the
fields. `field-info/1` returns the same metadata but only for a single
field. So if we called `field-info` with `name` we would get::

    {name,2,undefined}

Future Directions
~~~~~~~~~~~~~~~~~

There is still a lot that can be added to records. Things like

* Pre and Post hook functions
* Types and automatic type validators

and more. However, the core defined here shouldn't change
significantly.
