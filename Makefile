default: test

SHELL = bash
dirs = {bin,libexec}
prefix ?= /usr/local

dirs:; mkdir -p $(prefix)/$(dirs)
files = $(shell ls -d $(dirs)/*)
install:; cp -r -n $(dirs) $(prefix)
link: dirs; for x in $(files); do ln -s `pwd`/$$x $(prefix)/$$x; done
uninstall:; rm -r $(addprefix $(prefix)/,$(files))

test:; grep -H '#!/\(bin/\|usr/bin/env ba\)sh' libexec/*/* | \
cut -d: -f1 | xargs shellcheck -e SC1090
