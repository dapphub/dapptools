PATH := bin:$(PATH)

prefix = /usr/local
sources = $(filter-out %.actual %.ok,$(wildcard *) $(wildcard */*))

default: test

test: $(patsubst %.expected,%.ok,$(wildcard t/*.expected))
t/%.ok: t/% $(sources); seth --test $<

link:; ln -s `pwd`/bin/* $(prefix)/bin
install:; install `pwd`/bin/* $(prefix)/bin

clean:; rm -f t/*.{ok,actual}
