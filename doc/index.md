---
layout: default
title: Joxa
---

Introduction
------------

Joxa (pronounced like "jocksah") is a recent dialect of the Lisp
programming language. It is a general-purpose language supporting
interactive development that encourages a functional programming
style, and simplifies multithreaded programming.

Joxa runs on the Erlang Virtual Machine. Like other Lisps, Joxa treats
code as data and has a full unhygienic macro system.

**NOTE**: At the moment Joxa is a an Alpha product. It is used in
production, and fully self hosting. However, you should expect to find
bugs and hopefully report and/or fix those bugs. There are also still
a number of things that need to be done as far as libraries and the
like. We encourage you to use Joxa and participate in the community.
however, you should expect to run into issues.


Getting Started
---------------

You will probably want to start with
[Syntax](https://github.com/erlware/joxa/wiki/Syntax) the
[FAQ](https://github.com/erlware/joxa/wiki/FAQ).
documentation.

At the moment Joxa shares its mailing list with the other erlware
projects. If the traffic gets high that might change. For now, to
participate with the community sign up at.

* [Erlware Questions](http://groups.google.com/group/erlware-questions)
* [Erlware Dev](http://groups.google.com/group/erlware-dev)


Motivation
----------

The Erlang/OTP system is a robust, scalable, pragmatic programming
system that allows for the relatively simple creating of robust fault
tolerant systems. However, the syntax of Erlang, while simple, is
distracting from the language. The Author wishes to have the
productive, flexible syntax of lisp on top of the powerful Erlang VM.


Goals
-----

* Joxa should have a clear and simple semantics.
* Joxa should include as few special forms as possible; Most complex
  forms should be built in Joxa itself.
* Joxa should have a consistent runtime environment for both macros
  and code.
* Joxa should support and facilitate incremental development
* Joxa should integrate seamlessly into existing Erlang Systems.

