default: test

SHELL = bash
dirs = {bin,libexec,resources}
prefix ?= /usr/local

dirs:; mkdir -p $(prefix)/$(dirs)
files = $(shell ls -d $(dirs)/*)
install:; cp -r -n $(dirs) $(prefix)
link: uninstall dirs; for x in $(files); do \
ln -s `pwd`/$$x $(prefix)/$$x; done
uninstall:; rm -rf $(addprefix $(prefix)/,$(files))

test:
	set -e; \
	for x in libexec/*/*; do [[ -f $$x ]] && files+=($$x); done; \
	! grep '^#!/bin/sh' "$${files[@]}"; \
	grep '^#!/usr/bin/env bash' "$${files[@]}" | \
	cut -d: -f1 | xargs shellcheck --exclude=2001

.PHONY: default dirs install link uninstall test
