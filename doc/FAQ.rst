Frequently Asked Questions
**************************

Joxa is a very small functional language. Its actually designed less
to be a language as a tool set in which to build domain specific
languages through the use of macros and libraries.

Though it is based on the Erlang VM it is not, and has no intention of
being, Erlang.

### What is the difference between Joxa and LFE (both Lisps for the Erlang VM)

This is best explained in the following post:
http://ericbmerritt.posterous.com/differences-between-joxa-and-lfe


### How Do You Create Mutually Recursive Functions

All functions in Joxa have to be declared before they can be used. For
recursive functions this works fine, however, for two functions that
recurse on each other there doesn't seem to be much you can do.

#### Type Specs are your answer

Do a `defspec` of the function before using it. Specs, aside from
providing type information to the compiler, also serve as a
pre-declaration. For example, lets say you had this function:


    (defn even? (number)
      (case number
          (0
              :true)
          (_
              (odd? (- (erlang/abs number) 1)))))

    (defn odd? (number)
       (case number
           (0
               :false)
           (_
               (even? (- (erlang/abs number) 1)))))

This obviously wont work because `odd?` will not be declared when
`even?` is defined. You can get around this problem by declaring a `defspec` for `odd?`.

    (defspec odd? ((erlang/integer)) (erlang/boolean))

    (defn even? (number)
      (case number
          (0
              :true)
          (_
              (odd? (- (erlang/abs number) 1)))))

    (defn odd? (number)
       (case number
           (0
               :false)
           (_
               (even? (- (erlang/abs number) 1)))))

With this it works because you have declared your intent to implement
`odd?` on which `even?` depends.

### Will compiler.jxa ever be able to use macros?

Probably not, its a problem in the erts code loading scheme. Macros
take iterative compilation that is, each form needs to be available at
compile time so you have to compile each form and load it
individually.  When you load the compiler, it overrides the
joxa.compiler module currently loaded and since the new thing is
incomplete it breaks.

I think there might be some possiblitiy using of the new/old positions
in the code loader but that is a long shot. So for the compiler, and
the compiler only, macros are not usable.  Thats why the bootstrap
flag is there it aborts iterative compilation and just does it all in
one fell swoop.
