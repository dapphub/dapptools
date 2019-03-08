default: test
server:; python -m SimpleHTTPServer

SHELL = bash
dirs = {bin,libexec}
prefix ?= /usr/local

dirs:; mkdir -p $(prefix)/$(dirs)
files = $(shell ls -d $(dirs)/*)
install:; cp -r -n $(dirs) $(prefix)
link: uninstall dirs; for x in $(files); do \
ln -s `pwd`/$$x $(prefix)/$$x; done
uninstall:; rm -rf $(addprefix $(prefix)/,$(files))

test:; ! grep '^#!/bin/sh' libexec/*/* && \
grep '^#!/usr/bin/env bash' libexec/*/* | \
cut -d: -f1 | xargs shellcheck

.PHONY: default server dirs install link uninstall test
