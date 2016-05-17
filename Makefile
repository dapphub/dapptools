PATH := bin:$(PATH)

prefix = /usr/local
sources = $(filter-out %.ok,$(wildcard *) $(wildcard */*))

default: test

test: $(patsubst t/%.t,t/%.ok,$(wildcard t/*.t))
t/%.ok: t/%.t $(sources); $< && touch $@

link:; ln -s `pwd`/bin/* $(prefix)/bin
install:; install `pwd`/bin/* $(prefix)/bin

clean:; rm -f t/*.ok
