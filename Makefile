SINAN=$(shell which sinan)
REBAR=$(shell which rebar)

ifeq ($(SINAN),)
	COMMAND=$(REBAR) get-deps && $(REBAR) compile
else
	COMMAND=$(SINAN) build
endif

all:
	@$(COMMAND)
