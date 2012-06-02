VSN=0.0.8a
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

# Bootstrap Directories In Build
JOXA_BOOTSTRAP_DIR=$(abspath _build/bootstrap)
BOOTSTRAP_LIBDIR=$(JOXA_BOOTSTRAP_DIR)/lib
BOOTSTRAP_APPDIR=$(BOOTSTRAP_LIBDIR)/joxa-$(VSN)
BOOTSTRAP_BEAMDIR=$(BOOTSTRAP_APPDIR)/ebin

# Location of the support makefiles
BUILD_SUPPORT=$(CURDIR)/build-support
.SUFFIXES:
.SUFFIXES:.jxa

include $(BUILD_SUPPORT)/core-build.mkf
include $(BUILD_SUPPORT)/doc.mkf
