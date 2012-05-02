VSN=0.0.7a
ERL=$(shell which erl)
ERLC=$(shell which erlc)

# Project Directories (local to $(CURDIR))

SRCDIR=$(abspath $(CURDIR)/src)
TESTDIR=$(abspath $(CURDIR)/test)
PRIVDIR=$(abspath $(CURDIR)/priv)

# Build Directories In Build
JOXA_BUILD_DIR=$(abspath _build/joxa)
LIBDIR=$(JOXA_BUILD_DIR)/lib
APPDIR=$(LIBDIR)/joxa-$(VSN)
BEAMDIR=$(APPDIR)/ebin

# Location of the support makefiles
BUILD_SUPPORT=$(CURDIR)/build-support
.SUFFIXES:
.SUFFIXES:.jxa

include $(BUILD_SUPPORT)/core-build.mkf
include $(BUILD_SUPPORT)/debian.mkf
include $(BUILD_SUPPORT)/doc.mkf