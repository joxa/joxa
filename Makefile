VSN=0.0.2a
ERL=$(shell which erl)
SRCDIR=$(abspath ./src)
PRIVDIR=$(abspath ./priv)
BEAMDIR=$(APPDIR)/ebin
LOCAL_BIN=_build/bin/
BINDIR=$(DESTDIR)/usr/bin
JOXA_BUILD_DIR=$(abspath _build/joxa)

LOCAL_SINAN=_build/bin/sinan
TMPDIR=./_build/tmp
LOCAL_DEPS=$(TMPDIR)/deps.tar.gz

LIBDIR=$(abspath $(JOXA_BUILD_DIR)/lib)
APPDIR=$(LIBDIR)/joxa-$(VSN)
INSTALL_TARGET=$(DESTDIR)/usr/lib/erlang/lib/joxa-$(VSN)
TARBALL=../joxa_$(VSN).orig.tar.gz

ERLFLAGS=-noshell -pa $(APPDIR)/ebin

COMP= $(ERL) $(ERLFLAGS) -s 'joxa.compiler' main \
      -extra

BEAMS= $(BEAMDIR)/joxa/compiler.beam \
	$(BEAMDIR)/joxa/core.beam \
	$(BEAMDIR)/joxa/shell.beam \
        $(BEAMDIR)/joxa.beam \
	$(BEAMDIR)/joxa/records.beam

.SUFFIXES:
.SUFFIXES:.jxa
.PHONY:all test_bootstrap pre_bootstrap bootstrap setup do-changeover clean \
	test build cucumber shell escript pre_build setup

all:
	@echo **NOTE****NOTE****NOTE****NOTE****NOTE****NOTE****NOTE****NOTE**
	@echo
	@echo   To build joxa from scratch you need a new version of sinan.
	@echo   All of the commands make sure sinan is in place and ready.
	@echo
	@echo   If you would like you can get things setup by running
	@echo   'make setup' when you have a net connection. That should do the
	@echo   right thing until you run 'make clean' again.
	@echo
	@echo   To get a joxa binary do 'make escript'. This will put the
	@echo   binary in _build/joxa/escript that may then be copied to your path.
	@echo   to build the joxa OTP Application you can simply do 'make build'
	@echo
	@echo **NOTE****NOTE****NOTE****NOTE****NOTE****NOTE****NOTE****NOTE**

$(LIBDIR):
	mkdir -p $(LIBDIR)

$(TMPDIR):
	mkdir -p $(TMPDIR)

$(LOCAL_BIN):
	mkdir -p $(LOCAL_BIN)

$(LOCAL_SINAN): $(LOCAL_BIN)
	wget --progress=bar https://github.com/downloads/erlware/sinan/sinan -O $(LOCAL_SINAN)
	touch $(LOCAL_SINAN)
	chmod 777 $(LOCAL_SINAN)

$(LOCAL_DEPS): $(TMPDIR) $(LIBDIR)
	wget --progress=bar https://github.com/downloads/erlware/sinan/deps.tar.gz -O $(LOCAL_DEPS)
	touch $(LOCAL_DEPS)
	tar xzf $(LOCAL_DEPS) --directory=$(LIBDIR)

$(TMPDIR)/bootstrap_test.jxa: $(TMPDIR)
	cp $(SRCDIR)/joxa/compiler.jxa $(TMPDIR)/bootstrap_test.jxa

$(BEAMDIR)/joxa:
	mkdir -p $(BEAMDIR)/joxa

$(BEAMDIR)/joxa/bootstrap_compiler.beam: $(BEAMDIR)/joxa $(SRCDIR)/jxa_bootstrap.erl
	$(ERL) $(ERLFLAGS) -s jxa_bootstrap do_bootstrap \
	${BEAMDIR}/joxa/bootstrap_compiler.beam joxa.bootstrap_compiler -s init stop

$(BEAMDIR)/joxa/compiler.beam: $(SRCDIR)/joxa/compiler.jxa $(BEAMDIR)/joxa/bootstrap_compiler.beam
	$(ERL) $(ERLFLAGS) -s joxa.bootstrap_compiler main \
	-extra --bootstrap -o $(BEAMDIR) $(SRCDIR)/joxa/compiler.jxa

$(BEAMDIR)/joxa/%.beam: $(SRCDIR)/joxa/%.jxa
	$(COMP) -o $(BEAMDIR) $?

$(BEAMDIR)/%.beam: $(SRCDIR)/%.jxa
	$(COMP) -o $(BEAMDIR) $?

setup: $(LOCAL_DEPS) $(LOCAL_SINAN)

pre-build: $(LOCAL_SINAN)
	$(LOCAL_SINAN) build

build: pre-build $(BEAMS)

shell: build
	$(ERL) $(ERLFLAGS) -s joxa main

clean:
	rm -f ../joxa_*.debian.tar.gz
	rm -f ../joxa_*.dsc
	rm -f ../joxa_*.build
	rm -f ../joxa_*.changes
	rm -rf debian/patches
	rm -rf _build
	rm -rf erl_crash.dump

testall: build
	$(LOCAL_SINAN) cucumber; \
	$(LOCAL_SINAN) eunit; \
	$(LOCAL_SINAN) proper

cucumber: build
	$(LOCAL_SINAN) cucumber

escript: build
	$(LOCAL_SINAN) escript

test_bootstrap: $(BEAMDIR)/joxa/compiler.beam $(TMPDIR)/bootstrap_test.jxa
	sed -i 's/joxa\.compiler/bootstrap_test/g' $(TMPDIR)/bootstrap_test.jxa
	`which time` $(ERL) $(ERLFLAGS) -s joxa.compiler main \
	 -extra --bootstrap -o $(TMPDIR) $(TMPDIR)/bootstrap_test.jxa

pre_bootstrap: test_bootstrap
	$(ERL) $(ERLFLAGS) -s joxa.compiler main \
	-extra --bootstrap -o $(BEAMDIR) $(SRCDIR)/joxa/compiler.jxa
	$(ERL) $(ERLFLAGS) -s joxa.compiler main \
	-extra --bootstrap --ast -o $(BEAMDIR) $(SRCDIR)/joxa/compiler.jxa
	sed -i "s/'joxa\.compiler'/Name/g" $(BEAMDIR)/joxa/compiler.ast
	sed  '/<<<<REPLACE-THIS-WITH-AST>>>>/r $(BEAMDIR)/joxa/compiler.ast' $(PRIVDIR)/jxa_bootstrap.tmpl \
	| sed '/<<<<REPLACE-THIS-WITH-AST>>>>/d' > $(SRCDIR)/jxa_bootstrap.erl

bootstrap: clean build pre_bootstrap $(BEAMS)
	$(LOCAL_SINAN) cucumber; \
	$(LOCAL_SINAN) eunit; \
	$(LOCAL_SINAN) proper

##
## Debian packaging support for joxa
##

$(TARBALL):
	git archive --format=tar --prefix=joxa/ HEAD | gzip > $(TARBALL)

internal-build-deb:
	sinan build

bootstrap-deb: internal-build-deb pre_bootstrap $(BEAMS)
	sinan cucumber; \
	sinan eunit; \
	sinan proper

escript-deb: bootstrap-deb
	sinan escript

testall-deb:
	sinan cucumber; \
	sinan eunit; \
	sinan proper

install-deb:
	mkdir -p $(INSTALL_TARGET)
	mkdir -p $(BINDIR)
	cp -r $(APPDIR)/* $(INSTALL_TARGET)
	cp -r _build/joxa/escript/joxa $(BINDIR)

build-deb: $(TARBALL)
	rm -f ../joxa_*.deb
	rm -f ../joxa_*.changes
	pdebuild
	debuild -S

publish-ppa: build-deb
	dput -f ppa:afiniate/ppa ../joxa_$(VSN)_source.changes
