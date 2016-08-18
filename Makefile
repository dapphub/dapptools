default: test

SHELL = bash
dirs = {bin,libexec}
image ?= dbrock/seth
prefix ?= /usr/local

dirs:; mkdir -p $(prefix)/$(dirs)
files = $(shell ls -d $(dirs)/*)
install:; cp -r -n $(dirs) $(prefix)
link: dirs; for x in $(files); do ln -s `pwd`/$$x $(prefix)/$$x; done
uninstall:; rm -r $(prefix)/$(files)

build:; docker build -t $(image) .
run = docker run --rm -it -v `pwd`:`pwd`:ro -w `pwd` $(image)
system-test: install-test link-test
test: build; $(run) make system-test
shell:; $(run) $(SHELL)
run:; $(run) sh -c 'make -s install && $(cmd)'

install-test:; $(MAKE) install run-tests uninstall
link-test:; $(MAKE) link run-tests uninstall
run-tests:; for t in t/*; do (set -x; $$t); done
