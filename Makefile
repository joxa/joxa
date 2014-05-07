VSN=0.1.0
ERL=$(shell which erl)
ERLC=$(shell which erlc)

ifeq ($(OS),Windows_NT)
    REBAR=rebar.cmd
    WERL=$(shell cygpath -m $(shell which werl))
else
    REBAR=rebar
endif

CURDIR=.

ifeq ($(REBAR),)
	$(error "Rebar not available on this system")
endif
# Project Directories (local to $(CURDIR))

SRCDIR=src
TESTDIR=test
PRIVDIR=priv

# Build Directories In Build
APPDIR=.
BEAMDIR=$(APPDIR)/ebin

# Bootstrap Directories In Build
JOXA_BOOTSTRAP_DIR=.bootstrap

# Location of the support makefiles
BUILD_SUPPORT=$(CURDIR)/build-support
.SUFFIXES:
.SUFFIXES:.jxa

include $(BUILD_SUPPORT)/core-build.mkf
include $(BUILD_SUPPORT)/doc.mkf

clean: jxa-clean doc-clean

distclean: jxa-distclean doc-distclean

windows-shell:
	@echo "@echo off"                              > wjoxa.bat
	@echo "echo.Starting Erlang Emulater."        >> wjoxa.bat
	@echo "echo.To start joxa shell in werl, run" >> wjoxa.bat
	@echo "echo.  joxa:main()."                   >> wjoxa.bat
	@echo "echo.To finish joxa shell, run"        >> wjoxa.bat
	@echo "echo.  (require c)"                    >> wjoxa.bat
	@echo "echo.  (c/q)"                          >> wjoxa.bat
	@echo start $(subst /,\\,$(WERL)) $(subst /,\\,$(ERLCFLAGS))  >> wjoxa.bat
