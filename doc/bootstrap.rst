The Bootstrap Process
*********************

The bootstrapping of the system is a somewhat delicate balancing
act. The old version of the compiler must be used to build the new
version of the compiler in its entirety. However, the new version must
not be picked up by the Erlang runtime system until the entire system
has been built. To that end the new version of the compiler is built
into a temporary holding directory. Once all modules in the compiler
have been built they are moved into the ebin directory replacing the
current compiler.
