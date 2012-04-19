VSN=0.0.6a
ERL=$(shell which erl)
ERLC=$(shell which erlc)

#Project Directories (local to $(CURDIR))

SRCDIR=$(abspath $(CURDIR)/src)
TESTDIR=$(abspath $(CURDIR)/test)
PRIVDIR=$(abspath $(CURDIR)/priv)

# Build Directories In Build
JOXA_BUILD_DIR=$(abspath _build/joxa)
LIBDIR=$(JOXA_BUILD_DIR)/lib
APPDIR=$(LIBDIR)/joxa-$(VSN)
BEAMDIR=$(APPDIR)/ebin

.SUFFIXES:
.SUFFIXES:.jxa

include $(CURDIR)/build-support/core-build.mkf
include $(CURDIR)/build-support/debian.mkf