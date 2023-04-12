test:
	bash_unit integration/tests.sh

ci:
	FUZZ_RUNS=10000 TESTNET_SLEEP=90 bash_unit -r -f tap integration/tests.sh

.PHONY: test
