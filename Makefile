SINAN=$(shell which sinan)
REBAR=$(shell which rebar)

ifeq ($(SINAN),)
	COMMAND=$(REBAR) compile
else
	COMMAND=$(SINAN) build
endif

all:
	@$(COMMAND)
