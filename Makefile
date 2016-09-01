default: test

SHELL = bash
dirs = {bin,libexec}
image ?= nexusdev/seth
prefix ?= /usr/local

dirs:; mkdir -p $(prefix)/$(dirs)
files = $(shell ls -d $(dirs)/*)
install:; cp -r -n $(dirs) $(prefix)
link: dirs; for x in $(files); do ln -s `pwd`/$$x $(prefix)/$$x; done
uninstall:; rm -r $(addprefix $(prefix)/,$(files))

build:; docker build -t $(image) .
run = docker run --rm -it -v `pwd`:`pwd`:ro -w `pwd` $(image)
test: build; $(run) ./bin/seth --test
