CC=clang++
CFLAGS=-c -I/Users/blt/.kerl/installs/r16b02/usr/include/ -O2 -Wall -Wextra -pedantic -Weffc++ --std=c++11
LDFLAGS=-undefined dynamic_lookup -dynamiclib
SOURCES=cookie_bouncer.cpp
OBJECTS=$(SOURCES:.cpp=.o)
SHOBJ=cookie_bouncer.so

all: $(SOURCES) $(SHOBJ)

$(SHOBJ): $(OBJECTS)
	$(CC) $(LDFLAGS) $(OBJECTS) -shared -o $@

.cpp.o:
	$(CC) $(CFLAGS) $< -o $@
