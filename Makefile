all:
	@./rebar get-deps
	@./rebar compile

.PHONY: compile
compile:
	@./rebar compile
	
clean:
	@./rebar clean

