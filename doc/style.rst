Joxa Style Guide
****************

Copyright (C) 2012 Eric B. Merritt

CC BY-NC-SA 3.0

This work is licensed under a Creative Commons
[Attribution-NonCommercial-ShareAlike 3.0 Unported License](http://creativecommons.org/licenses/by-nc-sa/3.0/).

This work is derived from
[Riastradh's Lisp Style Rules](http://mumble.net/~campbell/scheme/style.txt)
by Talor R. Cambell

This is document describes a recommended style for Joxa. Its an
distilled from the best practices of the existing Lisp world and the
lessons learned in Joxa itself. Its not meant to be a rigid set of
rules for the style extremists. It is meant to help you get the most
out of Joxa.

This guide is written primarily as a collection of guidelines, with
rationale for each rule (If a guideline is missing rationale, please
inform the author!). Although a casual reader might go through and
read the guidelines without the rationale, such a reader would derive
little value from this guide. In order to apply the guidelines
meaningfully, their spirit must be understood; the letter of the
guidelines serves only to hint at the spirit.  The rationale is just
as important as the guideline.

Standard Rules
--------------

These are the standard rules for formatting Lisp code; they are
repeated here for completeness, although they are surely described
elsewhere.  These are the rules implemented in Emacs Lisp modes, and
utilities such as Paredit.

Parentheses
^^^^^^^^^^^

Terminology
"""""""""""

This guide avoids the term *parenthesis* except in the general use of
*parentheses* or *parenthesized*, because the word's generally
accepted definition, outside of the programming language, is a
statement whose meaning is peripheral to the sentence in which it
occurs, and *not* the typographical symbols used to delimit such
statements.

The balanced pair of typographical symbols that mark parentheses in
English text are *round brackets*, i.e. `(` and `)`.  There are
several other balanced pairs of typographical symbols, such as *square
brackets* (commonly called simply `brackets` in programming circles),
i.e. `[` and `]`; *curly braces* (sometimes called simply `braces`),
i.e. `{` and `}`; *angle brackets* (sometimes `brokets` (for `broken
brackets`)), i.e. `<` and `>`.

In any balanced pair of typographical symbols, the symbol that begins
the region delimited by the symbols is called the *opening bracket* or
the *left bracket*, such as `(` or`[` or `{` or `<`.  The symbol that
ends that region is called the *right bracket* or the *closing bracket*,
such as `>` or `}` or `]` or `)`.

Spacing
^^^^^^^
If any text precedes an opening bracket or follows a closing bracket,
separate that text from that bracket with a space.  Conversely, leave
no space after an opening bracket and before following text, or after
preceding text and before a closing bracket.

Unacceptable
""""""""""""
.. code-block:: clojure

    (foo(bar baz)quux)
    (foo ( bar baz ) quux)

Acceptable:
"""""""""""

.. code-block:: clojure

    (foo (bar baz) quux)

Rationale
"""""""""

This is the same spacing found in standard typography of western text.
It is more aesthetically pleasing.

Line Separation
^^^^^^^^^^^^^^^

Absolutely do *not* place closing brackets on their own lines.

Unacceptable
""""""""""""
.. code-block:: clojure

     (define (factorial x)
       (if (< x 2)
           1
           (* x (factorial (- x 1

                           )

                )

           )

       )

      )



Acceptable
""""""""""
.. code-block:: clojure

    (define (factorial x)
      (if (< x 2)
          1
          (* x (factorial (- x 1)))))

Rationale
"""""""""

The parentheses grow lonely if their closing brackets are all kept
separated and segregated.

Exceptions to the Above Rule Concerning Line Separation
"""""""""""""""""""""""""""""""""""""""""""""""""""""""

Do not heed this section unless you know what you are doing.  Its
title does *not* make the unacceptable example above acceptable.

When commenting out fragments of expressions with line comments, it may
be necessary to break a line before a sequence of closing brackets

.. code-block:: clojure

    (define (foo bar)
      (list (frob bar)
            (zork bar)
            ;; (zap bar)
            ))

Finally, it is acceptable to break a line immediately after an opening
bracket and immediately before a closing bracket for very long lists,
especially in files under version control.  This eases the maintenance
of the lists and clarifies version diffs.  Example

.. code-block:: clojure

    (define colour-names         ;Add more colour names to this list!
      '(
        blue
        cerulean
        green
        magenta
        purple
        red
        scarlet
        turquoise
        ))

Parenthetical Philosophy
^^^^^^^^^^^^^^^^^^^^^^^^

The actual bracket characters are simply lexical tokens to which
little significance should be assigned.  Lisp programmers do not
examine the brackets individually, or, Azathoth forbid, count
brackets; instead they view the higher-level structures expressed in
the program, especially as presented by the indentation.  Lisp is not
about writing a sequence of serial instructions; it is about building
complex structures by summing parts.  The composition of complex
structures from parts is the focus of Lisp programs, and it should be
readily apparent from the Lisp code.  Placing brackets haphazardly
about the presentation is jarring to a Lisp programmer, who otherwise
would not even have seen them for the most part.

Indentation and Alignment
"""""""""""""""""""""""""

The operator of any form, i.e. the first subform following the opening
round bracket, determines the rules for indenting or aligning the
remaining forms.  Many names in this position indicate special
alignment or indentation rules; these are special operators, macros,
or procedures that have certain parameter structures.

If the first subform is a non-special name, however, then if the
second subform is on the same line, align the starting column of all
following subforms with that of the second subform.  If the second
subform is on the following line, align its starting column with that
of the first subform, and do the same for all remaining subforms.

In general, Emacs will indent Lisp code correctly.  Run `C-M-q`
(indent-sexp) on any code to ensure that it is indented correctly, and
configure Emacs so that any non-standard forms are indented
appropriately.

Unacceptable
""""""""""""
.. code-block:: clojure

    (+ (sqrt -1)
      (* x y)
      (+ p q))

    (+
       (sqrt -1)
       (* x y)
       (+ p q))

Acceptable
""""""""""

.. code-block:: clojure

    (+ (sqrt -1)
       (* x y)
       (+ p q))

    (+
     (sqrt -1)
     (* x y)
     (+ p q))

Rationale
"""""""""

The columnar alignment allows the reader to follow the operands of any
operation straightforwardly, simply by scanning downward or upward to
match a common column.  Indentation dictates structure; confusing
indentation is a burden on the reader who wishes to derive structure
without matching parentheses manually.

Non-Symbol Indentation and Alignment
""""""""""""""""""""""""""""""""""""

The above rules are not exhaustive; some cases may arise with strange
data in operator positions.

Lists
^^^^^

Unfortunately, style varies here from person to person and from editor
to editor.  Here are some examples of possible ways to indent lists
whose operators are lists:

Questionable
""""""""""""

.. code-block:: clojure

    ((car x)                            ;Requires hand indentation.
       (cdr x)
       foo)

    ((car x) (cdr x)                    ;GNU Emacs
     foo)

Preferable
""""""""""

.. code-block:: clojure

    ((car x)                            ;Any Emacs
     (cdr x)
     foo)


Rationale
"""""""""

The operands should be aligned, as if it were any other procedure call
with a name in the operator position; anything other than this is
confusing because it gives some operands greater visual distinction,
allowing others to hide from the viewer's sight.  For example, the
questionable indentation

.. code-block:: clojure

    ((car x) (cdr x)
     foo)

can make it hard to see that `foo` and `(cdr x)` are both operands here at
the same level.  However, GNU Emacs will generate that indentation by
default.

Strings
^^^^^^^

If the form in question is meant to be simply a list of literal data,
all of the subforms should be aligned to the same column, irrespective
of the first subform.

Unacceptable
""""""""""""
.. code-block:: clojure

    ("foo" "bar" "baz" "quux" "zot"
           "mumble" "frotz" "gargle" "mumph")

Questionable, but acceptable
""""""""""""""""""""""""""""

.. code-block:: clojure

    (3 1 4 1 5 9 2 6 5 3 5 8 9 7 9 3 2 3 8 4 6 2 6 4
       3 3 8 3 2 7 9 5 0 2 8 8 4 1 9 7 1 6 9 3 9 9 3)

Acceptable
""""""""""

.. code-block:: clojure

    ("foo" "bar" "baz" "quux" "zot"
     "mumble" "frotz" "gargle" "mumph")

    ("foo"
      "bar" "baz" "quux" "zot"
      "mumble" "frotz" "gargle" "mumph")

Rationale
"""""""""

Seldom is the first subform distinguished for any reason, if it is a
literal; usually in this case it indicates pure data, not code.  Some
editors and pretty-printers, however, will indent unacceptably in the
example given unless the second subform is on the next line anyway,
which is why the last way to write the fragment is usually best.

Names
^^^^^
Naming is subtle and elusive.  Bizarrely, it is simultaneously
insignificant, because an object is independent of and unaffected by
the many names by which we refer to it, and also of supreme
importance, because it is what programming -- and, indeed, almost
everything that we humans deal with -- is all about.  A full
discussion of the concept of name lies far outside the scope of this
document, and could surely fill not even a book but a library.

Symbolic names are written with English words separated by hyphens.
Scheme and Common Lisp both fold the case of names in programs;
consequently, camel case is frowned upon, and not merely because it is
ugly.  Underscores are unacceptable separators except for names that
were derived directly from a foreign language without translation.

Unacceptable
""""""""""""
.. code-block:: clojure

    XMLHttpRequest
    foreach
    append_map

Acceptable
""""""""""
.. code-block:: clojure

    xml-http-request
    for-each
    append-map

Funny Characters
^^^^^^^^^^^^^^^^

Question Marks: Predicates
""""""""""""""""""""""""""

Affix a question mark to the end of a name for a procedure whose
purpose is to ask a question of an object and to yield a boolean
answer.  Such procedures are called `predicates`.  Do not use a
question mark if the procedure may return any object other than a
boolean.

Examples
.. code-block:: clojure

    pair? procedure? proper-list?

Pronounce the question mark as if it were the isolated letter `p`.  For
example, to read the fragment `(pair? object)` aloud, say: `pair-pee
object.`

Exclamation Marks: Destructive Operations
"""""""""""""""""""""""""""""""""""""""""

Affix an exclamation mark to the end of a name for a procedure (or
macro) whose primary purpose is to modify an object. This is common in
lisps that support destructive operations. Joxa, of course, does
not. However, this syntax is useful in situations where the intent is
to modify an object.

Examples

.. code-block:: clojure

    set-car! append!

Pronounce the exclamation mark as `bang`.  For example, to read the
fragment (append! list tail) aloud, say: `append-bang list tail`.

Asterisks: Variants, Internal Routines
""""""""""""""""""""""""""""""""""""""
Affix an asterisk to the end of a name to make a variation on a theme
of the original name.

Example

.. code-block:: clojure

    let -> let*

Prefer a meaningful name over an asterisk; the asterisk does not
explain what variation on the theme the name means.


`with-` and `call-with-`: Dynamic State and Cleanup
"""""""""""""""""""""""""""""""""""""""""""""""""""

Prefix `WITH-` to any procedure that establishes dynamic state and
calls a nullary procedure, which should be the last (required)
argument.  The dynamic state should be established for the extent of
the nullary procedure, and should be returned to its original state
after that procedure returns.

Examples

.. code-block:: clojure

     with-input-from-file
     with-output-to-file

Prefix `call-with-` to any procedure that calls a procedure, which
should be its last argument, with some arguments, and is either
somehow dependent upon the dynamic state or continuation of the
program, or will perform some action to clean up data after the
procedure argument returns.  Generally, `CALL-WITH-` procedures should
return the values that the procedure argument returns, after
performing the cleaning action.

`call-with-input-file` and `call-with-output-file` both accept a
pathname and a procedure as an argument, open that pathname (for input
or output, respectively), and call the procedure with one argument, a
port corresponding with the file named by the given pathname.  After
the procedure returns, call-with-input-file and call-with-output-file
close the file that they opened, and return whatever the procedure
returned.

Generally, the distinction between these two classes of procedures is
that `call-with-...` procedures should not establish fresh dynamic
state and instead pass explicit arguments to their procedure arguments,
whereas `with-...` should do the opposite and establish dynamic state
while passing zero arguments to their procedure arguments.

Comments
^^^^^^^^

Write heading comments with at least four semicolons; see also the
section below titled 'Outline Headings'.

Write top-level comments with three semicolons.

Write comments on a particular fragment of code before that fragment
and aligned with it, using two semicolons.

Write margin comments with one semicolon.

The only comments in which omission of a space between the semicolon
and the text is acceptable are margin comments.

Examples

.. code-block:: clojure

    ;;;; Frob Grovel

    ;;; This section of code has some important implications:
    ;;;   1. Foo.
    ;;;   2. Bar.
    ;;;   3. Baz.

    (defn (fnord zarquon)
      ;; If zob, then veeblefitz.
      (quux zot
            mumble             ;Zibblefrotz.
            frotz))

General Layout
--------------

Contained in the rationale for some of the following rules are
references to historical limitations of terminals and printers, which
are now considered aging cruft of no further relevance to today's
computers.  Such references are made only to explain specific measures
chosen for some of the rules, such as a limit of eighty columns per
line, or sixty-six lines per page.  There is a real reason for each of
the rules, and this real reason is not intrinsically related to the
historical measures, which are mentioned only for the sake of
providing some arbitrary measure for the limit.

File Length
^^^^^^^^^^^

If a file exceeds five hundred twelve lines, begin to consider
splitting it into multiple files.  Do not write program files that
exceed one thousand twenty-four lines.  Write a concise but
descriptive title at the top of each file, and include no content in
the file that is unrelated to its title.

Rationale
"""""""""

Files that are any larger should generally be factored into smaller
parts.  (One thousand twenty-four is a nicer number than one
thousand.)  Identifying the purpose of the file helps to break it into
parts if necessary and to ensure that nothing unrelated is included
accidentally.

Top-Level Form Length
^^^^^^^^^^^^^^^^^^^^^

Do not write top-level forms that exceed twenty-one lines, except for
top-level forms that serve only the purpose of listing large sets of
data.  If a procedure exceeds this length, split it apart and give
names to its parts.  Avoid names formed simply by appending a number
to the original procedure's name; give meaningful names to the parts.

Rationale
"""""""""

Top-level forms, especially procedure definitions, that exceed this
length usually combine too many concepts under one name.  Readers of
the code are likely to more easily understand the code if it is
composed of separately named parts.  Simply appending a number to the
original procedure's name can help only the letter of the rule, not
the spirit, however, even if the procedure was taken from a standard
algorithm description.  Using comments to mark the code with its
corresponding place in the algorithm's description is acceptable, but
the algorithm should be split up in meaningful fragments anyway.

Rationale for the number twenty-one: Twenty-one lines, at a maximum of
eighty columns per line, fits in a GNU Emacs instance running in a
24x80 terminal.  Although the terminal may have twenty-four lines,
three of the lines are occupied by GNU Emacs: one for the menu bar
(which the author of this guide never uses, but which occupies a line
nevertheless in a vanilla GNU Emacs installation), one for the mode
line, and one for the minibuffer's window.  The writer of some code
may not be limited to such a terminal, but the author of this style
guide often finds it helpful to have at least four such terminals or
Emacs windows open simultaneously, spread across a twelve-inch laptop
screen, to view multiple code fragments.

Line Length
^^^^^^^^^^^

Do not write lines that exceed eighty columns, or if possible
seventy-two.

Rationale
"""""""""

Following multiple lines that span more columns is difficult for
humans, who must remember the line of focus and scan right to left
from the end of the previous line to the beginning of the next line;
the more columns there are, the harder this is to do.  Sticking to a
fixed limit helps to improve readability.

Rationale for the numbers eighty and seventy-two: It is true that we
have very wide screens these days, and we are no longer limited to
eighty-column terminals; however, we ought to exploit our wide screens
not by writing long lines, but by viewing multiple fragments of code
in parallel, something that the author of this guide does very often.
Seventy-two columns leave room for several nested layers of quotation
in email messages before the code reaches eighty columns.  Also, a
fixed column limit yields nicer printed output, especially in
conjunction with pagination; see the section 'Pagination' below.

Blank Lines
^^^^^^^^^^^

Separate each adjacent top-level form with a single blank line (i.e.
two line breaks). Do not place blank lines in the middle of a
procedure body, except to separate internal definitions; if there is a
blank line for any other reason, split the top-level form up into
multiple ones.

Rationale
"""""""""

More than one blank line is distracting and sloppy.  If the two
concepts that are separated by multiple blank lines are really so
distinct that such a wide separator is warranted, then they are
probably better placed on separate pages anyway; see the next section,
*Pagination*.


Dependencies
^^^^^^^^^^^^

When writing a file or module, minimize its dependencies.  If there
are too many dependencies, consider breaking the module up into
several parts, and writing another module that is the sum of the parts
and that depends only on the parts, not their dependencies.

Rationale
"""""""""

A fragment of a program with fewer dependencies is less of a burden on
the reader's cognition.  The reader can more easily understand the
fragment in isolation; humans are very good at local analyses, and
terrible at global ones.

Naming
^^^^^^

This section requires an elaborate philosophical discussion which the
author is too ill to have the energy to write at this moment.

Compose concise but meaningful names.  Do not cheat by abbreviating
words or using contractions.

Rationale
"""""""""

Abbreviating words in names does not make them shorter; it only makes
them occupy less screen space.  The reader still must understand the
whole long name.  This does not mean, however, that names should
necessarily be long; they should be descriptive.  Some long names are
more descriptive than some short names, but there are also descriptive
names that are not long and long names that are not descriptive.  Here
is an example of a long name that is not descriptive, from SchMUSE, a
multi-user simulation environment written in MIT Scheme:

.. code-block:: clojure

    frisk-descriptor-recursive-subexpr-descender-for-frisk-descr-env

Not only is it long (sixty-four characters) and completely
impenetrable, but halfway through its author decided to abbreviate
some words as well!

Do not write single-letter variable names.  Give local variables
meaningful names composed from complete English words.

Rationale
"""""""""

It is tempting to reason that local variables are invisible to other
code, so it is OK to be messy with their names.  This is faulty
reasoning: although the next person to come along and use a library
may not care about anything but the top-level definitions that it
exports, this is not the only audience of the code.  Someone will also
want to read the code later on, and if it is full of impenetrably
terse variable names without meaning, that someone will have a hard
time reading the code.

Give names to intermediate values where their expressions do not
adequately describe them.

Rationale
"""""""""

An `expression` is a term that expresses some value.  Although a
machine needs no higher meaning for this value, and although it should
be written to be sufficiently clear for a human to understand what it
means, the expression might mean something more than just what it says
where it is used.  Consequently, it is helpful for humans to see names
given to expressions.

**Example**

A hash table maps foos to bars; `(dict/get dict foo :false)` expresses
the datum that dict maps foo to, but that expression gives the reader
no hint of any information concerning that datum.  `(let ((bar
(dict/get dict foo :false))) ...)` gives a helpful name for the reader
to understand the code without having to find the definition of
HASH-TABLE.

Index variables such as i and j, or variables such as A and D naming
the car and cdr of a pair, are acceptable only if they are completely
unambiguous in the scope.

Avoid functional combinators, or, worse, the point-free (or
`point-less`) style of code that is popular in the Haskell world.  At
most, use function composition only where the composition of functions
is the crux of the idea being expressed, rather than simply a
procedure that happens to be a composition of two others.

Rationale
"""""""""

Tempting as it may be to recognize patterns that can be structured as
combinations of functional combinators -- say, 'compose this procedure
with the projection of the second argument of that other one', or
`(compose foo (project 2 bar))` --, the reader of the code must
subsequently examine the elaborate structure that has been built up to
obscure the underlying purpose.  The previous fragment could have been
written `(fn (a b) (foo (bar b)))`, which is in fact shorter, and
which tells the reader directly what argument is being passed on to
what, and what argument is being ignored, without forcing the reader
to search for the definitions of foo and bar or the call site of the
final composition.  The explicit fragment contains substantially more
information when intermediate values are named, which is very helpful
for understanding it and especially for modifying it later on.

The screen space that can be potentially saved by using functional
combinators is made up for by the cognitive effort on the part of the
reader.  The reader should not be asked to search globally for usage
sites in order to understand a local fragment.  Only if the structure
of the composition really is central to the point of the narrative
should it be written as such.  For example, in a symbolic integrator
or differentiator, composition is an important concept, but in most
code the structure of the composition is completely irrelevant to the
real point of the code.

If a parameter is ignored, give it a meaningful name nevertheless and
say that it is ignored; do not simply call it 'ignored'.

When naming top-level bindings, assume namespace partitions unless in a
context where they are certain to be absent.  Do not write explicit
namespace prefixes, such as foo/bar for an operation BAR in a module
foo, unless the names will be used in a context known not to have any
kind of namespace partitions.

Rationale
"""""""""
Explicit namespace prefixes are ugly, and lengthen names without
adding much semantic content.  Joxa has its package system to separate
the namespaces of names.  It is better to write clear names which can
be disambiguated if necessary, rather than to write names that assume
some kind of disambiguation to be necessary to begin with.
Furthermore, explicit namespace prefixes are inadequate to cover name
clashes anyway: someone else might choose the same namespace prefix.
Relegating this issue to a module system removes it from the content
of the program, where it is uninteresting.

Comments
^^^^^^^^

Write comments only where the code is incapable of explaining itself.
Prefer self-explanatory code over explanatory comments.  Avoid
'literate programming' like the plague.

Rationale
"""""""""

If the code is often incapable of explaining itself, then perhaps it
should be written in a more expressive language.  This may mean using
a different programming language altogether, or, since we are talking
about Lisp, it may mean simply building a combinator language or a
macro language for the purpose.

Attribution
-----------

This guide was derived from

Riastradh's Lisp Style Rules by Taylor R. Campbell

licensed under:

This work is licensed under a
[Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License]
(http://creativecommons.org/licenses/by-nc-sa/3.0/)
