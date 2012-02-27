VSN=0.0.2a
ERL=$(shell which erl)
SRCDIR=$(abspath ./src)
PRIVDIR=$(abspath ./priv)
BEAMDIR=$(APPDIR)/ebin
TMPDIR=./_build/tmp
BINDIR=$(DESTDIR)/usr/bin
JOXA_BUILD_DIR=$(abspath _build/joxa)
APPDIR=$(abspath $(JOXA_BUILD_DIR)/lib/joxa-$(VSN))
INSTALL_TARGET=$(DESTDIR)/usr/lib/erlang/lib/joxa-$(VSN)
TARBALL=../joxa_$(VSN).orig.tar.gz

REBAR_BROKEN_BIN=$(abspath ./ebin)
REBAR_BROKEN_DEPS=$(abspath ./deps)

ERLFLAGS=-noshell -env ERL_LIBS $(REBAR_BROKEN_DEPS) -pa $(REBAR_BROKEN_BIN) \
	-pa $(APPDIR)/ebin

SINAN=$(shell which sinan)
REBAR=$(shell which rebar)

COMP= $(ERL) $(ERLFLAGS) -s 'joxa.compiler' main \
      -extra

ifeq ($(SINAN),)
	COMMAND=$(REBAR) get-deps && $(REBAR) compile
else
	COMMAND=$(SINAN) build
endif

BEAMS= $(BEAMDIR)/joxa/compiler.beam $(BEAMDIR)/joxa/shell.beam \
       $(BEAMDIR)/joxa/core.beam $(BEAMDIR)/joxa.beam $(BEAMDIR)/joxa/records.beam

.SUFFIXES:
.SUFFIXES:.jxa
.PHONY:all test_bootstrap pre_bootstrap bootstrap setup do-changeover clean \
	test build cucumber shell escript

all: build $(BEAMS)

build:
	@$(COMMAND)

clean:
	sinan clean

setup:
	sinan clean; \
        sinan build

testall: $(BEAMS)
	sinan cucumber; \
	sinan eunit; \
	sinan proper

cucumber: $(BEAMS)
	sinan cucumber

shell: $(BEAMS)
	`which rlwrap` sinan shell

escript: $(BEAMS)
	sinan escript

$(TMPDIR)/bootstrap_test.jxa:
	mkdir -p $(TMPDIR)
	cp $(SRCDIR)/joxa/compiler.jxa $(TMPDIR)/bootstrap_test.jxa

test_bootstrap: build $(BEAMDIR)/joxa/compiler.beam $(TMPDIR)/bootstrap_test.jxa
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

bootstrap: setup pre_bootstrap $(BEAMS)
	sinan cucumber; \
	sinan eunit; \
	sinan proper

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

##
## Debian packaging support for joxa
##

$(TARBALL):
	git archive --format=tar --prefix=joxa/ HEAD | gzip > $(TARBALL)

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
