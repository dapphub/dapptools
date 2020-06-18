all:;
	dapp install ds-test
	./run-tests
	dapp uninstall ds-test
	git reset HEAD^^
	rm -rf lib
.PHONY: all
