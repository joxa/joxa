Installing Joxa
===============

The easiest way by far to install joxa is to simply
[download it from the github site](https://github.com/erlware/joxa/downloads). This
will give you an executable file that will serve as your shell and
compiler (see the manual). However, if you would like to build from
the repository there are a few things you should do.

TLDR
----

    $> make get-deps
    $> make test
    $> make escript
    $> mv ./_build/joxa/escript/joxa <someplace-in-the-path>

Get The Dependencies
--------------------

Joxa has several compile time dependencies and one two time dependencies.

### Compile Time Dependencies

* [Cucumberl](https://github.com/membase/cucumberl) at least version 0.0.5
* [Proper](https://github.com/manopapad/proper) at least version 1.0

### Runtime Dependencies

* [Getopt](https://github.com/jcomellas/getopt) at least version 0.4.2
* [Erlware Commons](https://github.com/erlware/erlware_commons) at
  least version 0.6.1

You can pull all of these down build them and put them somewhere in
your erlang path (ERL_LIBS). However, there is a rule in the make file
that will do this as well. Just run

    $> make get-deps

Running The Tests
-----------------

Make sure you run the tests, otherwise how will you know if it works?

    $> make test

Executable File (escript)
-------------------------

Finally build the escript. This will give you a binary executable that
you can use to run joxa.

    $> make escript

Then you can just mv the executable to some place in your path.

    $> mv ./_build/joxa/escript/joxa <someplace-in-the-path>


OTP Application
---------------

If you would like to have access to the OTP application to use as a
dependency you might want to move the built application to someplace
in your ERL_LIBS path. This is located in _build/joxa/lib/joxa-<vsn>
