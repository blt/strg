REBAR := `pwd`/rebar

all: compile

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean
	c_src/build.sh clean

.PHONY: compile
