test:
	pytest --hypothesis-show-statistics integration/diff-fuzz.py
	bash_unit integration/tests.sh

ci:
	export FUZZ_RUNS=10000
	pytest --hypothesis-show-statistics integration/diff-fuzz.py
	bash_unit integration/tests.sh

.PHONY: test
