default: link

SHELL = bash
dirs = {bin,libexec}
prefix ?= /usr/local

install: link
npmi:; npm install
dirs:; mkdir -p $(prefix)/$(dirs)
files = $(shell ls -d $(dirs)/*)
link: npmi dirs; for x in $(files); do ln -s `pwd`/$$x $(prefix)/$$x; done
uninstall:; rm -r $(addprefix $(prefix)/,$(files))
