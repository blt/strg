CC=c++
WARNINGS=-Wall -Weffc++ -pedantic  \
    -pedantic-errors -Wextra -Waggregate-return -Wcast-align \
    -Wcast-qual  -Wchar-subscripts  -Wcomment -Wconversion \
    -Wdisabled-optimization \
    -Werror -Wfloat-equal  -Wformat  -Wformat=2 \
    -Wformat-nonliteral -Wformat-security  \
    -Wformat-y2k \
    -Wimport  -Winit-self  -Winline \
    -Winvalid-pch -Wlong-long -Wmissing-braces \
    -Wmissing-field-initializers -Wmissing-format-attribute   \
    -Wmissing-include-dirs -Wmissing-noreturn \
    -Wpacked -Wparentheses  -Wpointer-arith \
    -Wredundant-decls -Wreturn-type \
    -Wsequence-point  -Wshadow -Wsign-compare  -Wstack-protector \
    -Wstrict-aliasing -Wstrict-aliasing=2 -Wswitch  -Wswitch-default \
    -Wswitch-enum -Wtrigraphs  -Wuninitialized \
    -Wunknown-pragmas  -Wunreachable-code -Wunused \
    -Wunused-function  -Wunused-label  -Wunused-parameter \
    -Wunused-value  -Wunused-variable  -Wvariadic-macros \
    -Wvolatile-register-var  -Wwrite-strings \
    -Wno-aggregate-return
CFLAGS=-c -I${ERL_ROOT}/usr/include/ -isystem include/ -MMD -MP -O2 ${WARNINGS} --std=c++11

UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
LDFLAGS=-shared -fPIC
endif
ifeq ($(UNAME), Darwin)
LDFLAGS=-undefined dynamic_lookup -dynamiclib
endif

SOURCES=erl_nif_strg.cpp
OBJECTS=$(SOURCES:.cpp=.o)
DEP=$(SOURCES:.cpp=.d)
-include $(DEP)
SHOBJ=strg.so

all: $(SOURCES) $(SHOBJ)

$(SHOBJ): $(OBJECTS)
	$(CC) $(LDFLAGS) $(OBJECTS) -shared -o $@

.cpp.o:
	$(CC) $(CFLAGS) $< -o $@

clean:
	@rm -f $(OBJECTS) $(DEP) $(SHOBJ)
	@rm -f *.beam

.PHONY: clean
