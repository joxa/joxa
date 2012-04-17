VSN=0.0.5a
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

# System install targes (for the packages)
BINDIR=$(DESTDIR)/usr/bin

ESCRIPT_DIR=$(JOXA_BUILD_DIR)/escript
ESCRIPT_TMP=$(CURDIR)/_build/tmp2
TMPDIR=$(CURDIR)/_build/tmp
LOCAL_DEPS=$(TMPDIR)/deps.tar.gz

INSTALL_TARGET=$(DESTDIR)/usr/lib/erlang/lib/joxa-$(VSN)
TARBALL=../joxa_$(VSN).orig.tar.gz

EBIN_DIRS=$(wildcard $(LIBDIR)/*/ebin)
ERLFLAGS=-noshell $(EBIN_DIRS:%= -pa %)
ERLCFLAGS=$(EBIN_DIRS:%= -pa %)

COMP= $(ERL) $(ERLFLAGS) -s 'joxa.compiler' main \
      -extra

SRCBEAMS= $(BEAMDIR)/joxa/compiler.beam \
	$(BEAMDIR)/joxa/core.beam \
	$(BEAMDIR)/joxa/shell.beam \
        $(BEAMDIR)/joxa.beam \
	$(BEAMDIR)/joxa/records.beam \
	$(BEAMDIR)/joxa/assert.beam \
	$(BEAMDIR)/joxa/eunit.beam

TESTBEAMS = $(BEAMDIR)/jxat_anon_fun.beam  \
	$(BEAMDIR)/jxat_examples.beam  \
	$(BEAMDIR)/jxat_module_info.beam \
	$(BEAMDIR)/jxat_rest_args.beam \
	$(BEAMDIR)/jxat_bare_module.beam \
	$(BEAMDIR)/jxat_featureful_module.beam \
	$(BEAMDIR)/jxat_nested_calls.beam  \
	$(BEAMDIR)/jxat_segfault_tests.beam \
	$(BEAMDIR)/jxat_binary.beam \
	$(BEAMDIR)/jxat_hello_world.beam \
	$(BEAMDIR)/jxat_parse.beam \
	$(BEAMDIR)/jxat_specs.beam \
	$(BEAMDIR)/jxat_case.beam \
	$(BEAMDIR)/jxat_implicit_do.beam \
	$(BEAMDIR)/jxat_path.beam  \
	$(BEAMDIR)/jxat_throws.beam \
	$(BEAMDIR)/jxat_core_add.beam \
	$(BEAMDIR)/jxat_incremental_compile.beam  \
	$(BEAMDIR)/jxat_peg.beam \
	$(BEAMDIR)/jxat_try.beam \
	$(BEAMDIR)/jxat_core_incr.beam  \
	$(BEAMDIR)/jxat_jxa_parser_proper.beam \
	$(BEAMDIR)/jxat_predicates.beam \
	$(BEAMDIR)/jxat_variable_fun_tests.beam \
	$(BEAMDIR)/jxat_ctx.beam \
	$(BEAMDIR)/jxat_let_support.beam  \
	$(BEAMDIR)/jxat_receive.beam   \
	$(BEAMDIR)/jxat_do_test.beam \
	$(BEAMDIR)/jxat_macros.beam   \
	$(BEAMDIR)/jxat_records.beam \
	$(BEAMDIR)/jxat_module_fun_line_support.beam \
	$(BEAMDIR)/jxat_assert.beam \
	$(BEAMDIR)/jxat_eunit.beam \
	$(BEAMDIR)/joxa/test-let-match.beam

.SUFFIXES:
.SUFFIXES:.jxa
.PHONY:all test_bootstrap pre_bootstrap bootstrap clean \
	test build proper eunit cucumber shell bare-escript \
	install-deb build-deb publish-ppa escript-deb

FEATURES=./features/*.feature

all: build

$(LOCAL_DEPS): $(TMPDIR) $(LIBDIR)
	wget --progress=bar https://github.com/downloads/erlware/sinan/deps.tar.gz -O $(LOCAL_DEPS)
	touch $(LOCAL_DEPS)
	tar xzf $(LOCAL_DEPS) --directory=$(LIBDIR)

$(ESCRIPT_DIR):
	mkdir -p $(ESCRIPT_DIR)

$(ESCRIPT_TMP):
	mkdir -p $(ESCRIPT_TMP)

$(LIBDIR):
	mkdir -p $(LIBDIR)

$(TMPDIR):
	mkdir -p $(TMPDIR)

$(TMPDIR)/bootstrap_test.jxa: $(TMPDIR)
	cp $(SRCDIR)/joxa/compiler.jxa $(TMPDIR)/bootstrap_test.jxa

$(BEAMDIR)/joxa:
	mkdir -p $(BEAMDIR)/joxa

$(BEAMDIR)/joxa/bootstrap_compiler.beam: $(BEAMDIR)/joxa $(BEAMDIR)/jxa_bootstrap.beam
	$(ERL) $(ERLFLAGS) -s jxa_bootstrap do_bootstrap \
	${BEAMDIR}/joxa/bootstrap_compiler.beam joxa.bootstrap_compiler -s init stop

$(BEAMDIR)/joxa/compiler.beam: $(SRCDIR)/joxa/compiler.jxa $(BEAMDIR)/joxa/bootstrap_compiler.beam
	$(ERL) $(ERLFLAGS) -s joxa.bootstrap_compiler main \
	-extra --bootstrap -o $(BEAMDIR) $(SRCDIR)/joxa/compiler.jxa

$(BEAMDIR)/joxa/%.beam: $(SRCDIR)/joxa/%.jxa
	$(COMP) -o $(BEAMDIR) $?

$(BEAMDIR)/%.beam: $(SRCDIR)/%.jxa
	$(COMP) -o $(BEAMDIR) $?

$(BEAMDIR)/%.beam: $(SRCDIR)/%.erl
	$(ERLC) $(ERLCFLAGS) -o $(BEAMDIR) $?

$(BEAMDIR)/%.beam: $(TESTDIR)/%.erl
	$(ERLC) $(ERLCFLAGS) -o $(BEAMDIR) $?

$(BEAMDIR)/joxa/%.beam: $(TESTDIR)/joxa/%.jxa
	$(COMP) -o $(BEAMDIR) $?

build: $(LOCAL_DEPS) $(SRCBEAMS) $(TESTBEAMS)

shell: build
	$(ERL) $(ERLFLAGS) -s joxa main -s init stop

clean:
	rm -f ../joxa_*.debian.tar.gz
	rm -f ../joxa_*.dsc
	rm -f ../joxa_*.build
	rm -f ../joxa_*.changes
	rm -rf debian/patches
	rm -rf _build
	rm -rf erl_crash.dump
	rm -rf ./usr

test: proper eunit cucumber

proper: $(SRCBEAMS) $(TESTBEAMS)
	for f in $(notdir $(basename $(TESTBEAMS))); do	\
	  set -e; \
	  echo Testing $$f;  \
	  $(ERL) $(ERLFLAGS) -eval "proper:module('$$f')" -s init stop; \
	done

eunit: $(SRCBEAMS) $(TESTBEAMS)
	for f in $(notdir $(basename $(TESTBEAMS))); do	\
	  set -e; \
	  echo Testing $$f;  \
	  $(ERL) $(ERLFLAGS) -eval "eunit:test('$$f')" -s init stop; \
	done

cucumber: $(SRCBEAMS) $(TESTBEAMS)
	for f in $(FEATURES) ; do	\
		set -e; \
		echo Testing $$f;  \
		$(ERL) $(ERLFLAGS) -eval "cucumberl:run(\"$$f\")" -s init stop; \
	done

bare-escript: $(ESCRIPT_DIR) $(ESCRIPT_TMP)
	cp -R $(LIBDIR)/* $(ESCRIPT_TMP)
	cd $(ESCRIPT_TMP); zip -r joxa.ez *; \
        $(ERL) $(ERLFLAGS) -eval "escript:create(\"joxa\", [shebang, {emu_args, []}, {archive, \"./joxa.ez\"}])" -s init stop
	chmod 777 $(ESCRIPT_TMP)/joxa
	mv $(ESCRIPT_TMP)/joxa $(ESCRIPT_DIR)/
	rm -rf $(ESCRIPT_TMP)


escript: build bare-escript

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

bootstrap: clean build pre_bootstrap $(SRCBEAMS) $(TESTBEAMS) proper cucumber eunit

##
## Debian packaging support for joxa
##

$(TARBALL):
	git archive --format=tar --prefix=joxa/ HEAD | gzip > $(TARBALL)

escript-deb: $(BEAMDIR)/joxa/compiler.beam $(SRCBEAMS) $(TESTBEAMS)  bare-escript

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
