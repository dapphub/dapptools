test:
	pytest --hypothesis-show-statistics integration/diff-fuzz.py
	bash_unit integration/tests.sh

ci:
	export FUZZ_RUNS=10000
	export TESTNET_SLEEP=90
	pytest --hypothesis-show-statistics integration/diff-fuzz.py
	bash_unit integration/tests.sh

.PHONY: test
