CC=clang++
CFLAGS=-c -I${ERL_ROOT}/usr/include/ -MMD -MP -O2 -Wall -Wextra -pedantic -Weffc++ --std=c++11

UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
LDFLAGS=-shared -fPIC
endif
ifeq ($(UNAME), Darwin)
LDFLAGS=-undefined dynamic_lookup -dynamiclib
endif

SOURCES=cookie_bouncer.cpp
OBJECTS=$(SOURCES:.cpp=.o)
DEP=$(SOURCES:.cpp=.d)
-include $(DEP)
SHOBJ=cookie_bouncer.so

all: $(SOURCES) $(SHOBJ)

$(SHOBJ): $(OBJECTS)
	$(CC) $(LDFLAGS) $(OBJECTS) -shared -o $@

.cpp.o:
	$(CC) $(CFLAGS) $< -o $@

clean:
	@rm -f $(OBJECTS) $(DEP) $(SHOBJ)
	@rm -f *.beam

.PHONY: clean
