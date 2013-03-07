Install
*******

If you are using Ubuntu its easiest to install Joxa from the
`PPA <https://launchpad.net/~afiniate/+archive/ppa>`_. This will get
everything setup for you.  For other distributions you simply need to
drop the Joxa executable in your path. That executable is an
``escript``. An escript is basically a binary executable. However, it
depends on the existence (on your machine) of the Erlang Virtual
Machine. So either install that
`now from source <http://www.erlang.org>`_ or install it from the
packaging system on your distribution.

If you are using Windows, install a recent Erlang (R15B or newer), add
Erlang's bin directory to your path, and drop ``joxa.cmd`` in there too.

How to properly install your project. Ideally, your project should be
installable via a common (simplistic) method: PyPI for Python, PEAR
for PHP, CPAN for Perl, RubyGems for Ruby, etc.
