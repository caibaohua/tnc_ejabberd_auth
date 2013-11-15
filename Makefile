.PHONY: rel deps

REBAR=./rebar

all: deps compile

compile: deps
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps
	@$(REBAR) check-deps

clean:
	@$(REBAR) clean

realclean: clean
	@$(REBAR) delete-deps

test:
	@$(REBAR) skip_deps=true ct