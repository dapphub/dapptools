SHELL = bash

all: image test
image = dbrock/seth
image:; docker build -t $(image) .

src = $(shell pwd)
run = docker run --rm -it -v $(src):$(src):ro -w $(src) $(image)
test:; $(run) sh -ec 'make install; for t in t/*; do (set -x; $$t); done'

# test:;    $(run) behave --stop -s
# wip:;     $(run) behave --stop -s --wip
# steps:;   $(run) behave --steps-catalog
# console:; $(run)

prefix = /usr/local
install:
	mkdir -p $(prefix)/{bin,libexec}
	cp bin/seth $(prefix)/bin/seth
	cp -r libexec/seth $(prefix)/libexec/seth
link:
	mkdir -p $(prefix)/{bin,libexec}
	ln -s `pwd`/bin/seth $(prefix)/bin/seth
	ln -s `pwd`/libexec/seth $(prefix)/libexec/seth
uninstall:
	rm -r $(prefix)/{bin,libexec}/seth
