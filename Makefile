PATH := bin:$(PATH)
prefix = /usr/local
sources = $(filter-out %.ok,$(wildcard *) $(wildcard */*))

default: test
clean:; rm -f t/.*.ok
test: $(patsubst %.t,%.ok,$(wildcard t/*.t))
%.ok: %.t $(sources); $< && touch $@

link:;    ln -s   `pwd`/bin/* $(prefix)/bin
install:; install `pwd`/bin/* $(prefix)/bin
