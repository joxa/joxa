VSN=0.0.1a
ERL=$(shell which erl)
SIN_BUILD_DIR=$(abspath _build/joxa)
APPDIR=$(abspath $(SIN_BUILD_DIR)/lib/joxa-$(VSN))
SRCDIR=$(abspath ./src)
PRIVDIR=$(abspath ./priv)
REBAR_BROKEN_BIN=$(abspath ./ebin)
REBAR_BROKEN_DEPS=$(abspath ./deps)
BEAMDIR=$(APPDIR)/ebin
TMPDIR=./_build/tmp
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
.PHONY:all bootstrap_test bootstrap_helper bootstrap setup do-changeover

all:
	@$(COMMAND)

bootstrap:  $(BEAMS)

setup:
	sinan clean; \
        sinan build

do-changeover: setup bootstrap_test bootstrap_helper
	sinan cucumber; \
	sinan eunit; \
	sinan proper

$(TMPDIR)/bootstrap_test.jxa:
	mkdir -p $(TMPDIR)
	cp $(SRCDIR)/joxa/compiler.jxa $(TMPDIR)/bootstrap_test.jxa

bootstrap_test: $(BEAMDIR)/joxa/compiler.beam $(TMPDIR)/bootstrap_test.jxa
	sed -i 's/joxa\.compiler/bootstrap_test/g' $(TMPDIR)/bootstrap_test.jxa
	`which time` $(ERL) $(ERLFLAGS) -s joxa.compiler main \
	 -extra --bootstrap -o $(TMPDIR) $(TMPDIR)/bootstrap_test.jxa

bootstrap_helper:
	$(ERL) $(ERLFLAGS) -s joxa.compiler main \
	-extra --bootstrap -o $(BEAMDIR) $(SRCDIR)/joxa/compiler.jxa
	$(ERL) $(ERLFLAGS) -s joxa.compiler main \
	-extra --bootstrap --ast -o $(BEAMDIR) $(SRCDIR)/joxa/compiler.jxa
	sed -i "s/'joxa\.compiler'/Name/g" $(BEAMDIR)/joxa/compiler.ast
	sed  '/<<<<REPLACE-THIS-WITH-AST>>>>/r $(BEAMDIR)/joxa/compiler.ast' $(PRIVDIR)/jxa_bootstrap.tmpl \
	| sed '/<<<<REPLACE-THIS-WITH-AST>>>>/d' > $(SRCDIR)/jxa_bootstrap.erl

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
