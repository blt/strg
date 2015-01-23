CC=clang++
CFLAGS=-c -O2 -Wall -Wextra -pedantic -Weffc++ --std=c++11
LDFLAGS=
SOURCES=cookie_bouncer.cpp
OBJECTS=$(SOURCES:.cpp=.o)
EXECUTABLE=cb

all: $(SOURCES) $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS)
	$(CC) $(LDFLAGS) $(OBJECTS) -o $@

.cpp.o:
	$(CC) $(CFLAGS) $< -o $@
