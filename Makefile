PATH := bin:$(PATH)

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
install:; install `pwd`/bin/* $(prefix)/bin
link:;    ln -s   `pwd`/bin/* $(prefix)/bin
uninstall:
	@echo Press enter to remove "$(prefix)/bin/seth*" \
        or Ctrl-C to cancel; read
	rm $(prefix)/bin/seth*
