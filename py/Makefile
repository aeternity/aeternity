BIN = $(abspath bin)
NOSE = $(BIN)/nosetests
PYTHON ?= $(BIN)/python
PYTHON_TESTS = $(abspath tests)
PIP ?= $(BIN)/pip
VIRTUALENV ?= virtualenv
VIRTUALENV_PYTHON ?= python2.7
UNAME_S = $(shell uname -s)
BLOCKS ?= 25

.PHONY: venv-present
venv-present:
ifeq ($(filter MINGW%,$(UNAME_S)),)
	@$(VIRTUALENV) --python=$(VIRTUALENV_PYTHON) -q .
else
	@echo "No virtualenv will be set up because it is not used on win32/msys2."
endif

.PHONY: env
env: venv-present
ifeq ($(filter MINGW%,$(UNAME_S)),)
	@. bin/activate && $(PIP) -q install -r requirements.txt
else
	@$(PIP) -q install -r requirements-win32.txt
endif

.PHONY: uats
uats:
	( cd $(PYTHON_TESTS) && $(NOSE)  --nocapture --logging-level=INFO -c $(PYTHON_TESTS)/nose.cfg --tc-file $(PYTHON_TESTS)/integration/setup.yaml --tc-format yaml $(PYTHON_TESTS)/integration/; )

.PHONY: single-uat
single-uat:
	( cd $(PYTHON_TESTS) && $(NOSE)  --nocapture --logging-level=INFO  -c $(PYTHON_TESTS)/nose.cfg --tc-file $(PYTHON_TESTS)/integration/setup.yaml --tc-format yaml $(PYTHON_TESTS)/integration/$(TEST_NAME); )

.PHONY: release-test
release-test:
	@$(PYTHON) $(PYTHON_TESTS)/release.py --workdir="$(WORKDIR)" --package=$(PACKAGE) --blocks=$(BLOCKS) --version=$(VER)

.PHONY: package-win32-test
package-win32-test:
	@$(PYTHON) $(PYTHON_TESTS)/package_win32.py --workdir="$(WORKDIR)" --packagespecfile=$(PACKAGESPECFILE)
