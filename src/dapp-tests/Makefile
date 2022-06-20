test:
	pytest --hypothesis-show-statistics integration/diff-fuzz.py
	bash_unit integration/tests.sh

ci:
	pytest --hypothesis-show-statistics integration/diff-fuzz.py
	FUZZ_RUNS=10000 TESTNET_SLEEP=1800 bash_unit integration/tests.sh

.PHONY: test
