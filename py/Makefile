BIN = bin
NOSE = $(BIN)/nosetests
PYTHON = $(BIN)/python
PYTHON_TESTS = tests
PIP = $(BIN)/pip


.PHONY: venv-present
venv-present:
	@virtualenv -q .

.PHONY: env
env: venv-present
	@. bin/activate && $(PIP) -q install -r requirements.txt 

.PHONY: websocket-test 
websocket-test:
	@$(PYTHON) $(PYTHON_TESTS)/ws_client.py --port 3114 --log INFO --handler ws_logic

.PHONY: uats
uats:
	@$(NOSE)  --nocapture -c $(PYTHON_TESTS)/nose.cfg --tc-file $(PYTHON_TESTS)/integration/setup.yaml --tc-format yaml $(PYTHON_TESTS)/integration/

.PHONY: single-uat
single-uat:
	@$(NOSE)  --nocapture -c $(PYTHON_TESTS)/nose.cfg --tc-file $(PYTHON_TESTS)/integration/setup.yaml --tc-format yaml $(PYTHON_TESTS)/integration/test_$(TEST_NAME).py

.PHONY: release-test
release-test:
	@$(PYTHON) $(PYTHON_TESTS)/release.py --workdir="$(WORKDIR)" --tarball=$(TARBALL) --blocks=100 --version=$(VER)

