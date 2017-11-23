CORE = rel/epoch/bin/epoch
VER := $(shell cat VERSION)

CT_TEST_FLAGS =
EUNIT_TEST_FLAGS =
ifdef SUITE
CT_TEST_FLAGS += --suite=$(SUITE)_SUITE
endif

ifdef GROUP
CT_TEST_FLAGS += --group=$(GROUP)
endif

ifdef TEST
CT_TEST_FLAGS += --case=$(TEST)
EUNIT_TEST_FLAGS += --module=$(TEST)
endif

PYTHON_DIR = py
PYTHON_BIN = $(PYTHON_DIR)/bin
NOSE = $(PYTHON_BIN)/nosetests
PYTHON = $(PYTHON_BIN)/python
PYTHON_TESTS = $(PYTHON_DIR)/tests
PIP = $(PYTHON_BIN)/pip


HTTP_APP = apps/aehttp
SWTEMP := $(shell mktemp -d 2>/dev/null || mktemp -d -t 'mytmpdir')


local-build: KIND=local
local-build: internal-build

local-start: KIND=local
local-start: internal-start

local-stop: KIND=local
local-stop: internal-stop

local-attach: KIND=local
local-attach: internal-attach

prod-build: KIND=prod
prod-build: internal-build

prod-start: KIND=prod
prod-start: internal-start

prod-stop: KIND=prod
prod-stop: internal-stop

prod-attach: KIND=prod
prod-attach: internal-attach

multi-start:
	@make dev1-start
	@make dev2-start
	@make dev3-start

multi-stop:
	@make dev1-stop
	@make dev2-stop
	@make dev3-stop

multi-clean:
	@make dev1-clean
	@make dev2-clean
	@make dev3-clean

dev1-build: KIND=dev1
dev1-build: internal-build

dev1-start: KIND=dev1
dev1-start: internal-start

dev1-stop: KIND=dev1
dev1-stop: internal-stop

dev1-attach: KIND=dev1
dev1-attach: internal-attach

dev1-clean: KIND=dev1
dev1-clean: internal-clean

dev2-start: KIND=dev2
dev2-start: internal-start

dev2-stop: KIND=dev2
dev2-stop: internal-stop

dev2-attach: KIND=dev2
dev2-attach: internal-attach

dev2-clean: KIND=dev2
dev2-clean: internal-clean

dev3-start: KIND=dev3
dev3-start: internal-start

dev3-stop: KIND=dev3
dev3-stop: internal-stop

dev3-attach: KIND=dev3
dev3-attach: internal-attach

dev3-clean: KIND=dev3
dev3-clean: internal-clean

dialyzer:
	@./rebar3 dialyzer

test:
	@./rebar3 do ct $(CT_TEST_FLAGS)

eunit:
	@./rebar3 do eunit $(EUNIT_TEST_FLAGS)

venv-present:
	@virtualenv -q $(PYTHON_DIR)

python-env: venv-present
	@. $(PYTHON_BIN)/activate && $(PIP) -q install -r $(PYTHON_DIR)/requirements.txt 

python-tests:
	@$(NOSE) --nocapture -c $(PYTHON_TESTS)/nose.cfg $(PYTHON_TESTS)

python-ws-test:
	@$(PYTHON) $(PYTHON_TESTS)/ws_client.py --port 3014 --log INFO --handler ws_logic

python-integration-tests:
	@$(NOSE)  --nocapture -c $(PYTHON_TESTS)/nose.cfg --tc-file $(PYTHON_TESTS)/integration/setup.yaml --tc-format yaml $(PYTHON_TESTS)/integration/

python-single-integration-test:
	@$(NOSE)  --nocapture -c $(PYTHON_TESTS)/nose.cfg --tc-file $(PYTHON_TESTS)/integration/setup.yaml --tc-format yaml $(PYTHON_TESTS)/integration/test_$(TEST_NAME).py

release-integration-test:
	@$(PYTHON) $(PYTHON_TESTS)/release.py --tarball=$(TARBALL) --maxheight=10 --version=$(VER)

python-download-chain:
	@$(PYTHON) $(PYTHON_TESTS)/chain_downloader.py --host=localhost --port=3013 --export_file=$(PYTHON_TESTS)/integration/data/bchain.txt

SWAGGER_CODEGEN_CLI_V = 2.2.3
SWAGGER_CODEGEN_CLI = swagger/swagger-codegen-cli-$(SWAGGER_CODEGEN_CLI_V).jar
SWAGGER_CODEGEN = java -jar $(SWAGGER_CODEGEN_CLI)

swagger: config/swagger.yaml $(SWAGGER_CODEGEN_CLI)
	$(SWAGGER_CODEGEN) generate -i $< -l erlang-server -o $(SWTEMP)
	@echo "Swagger tempdir: $(SWTEMP)"
	@cp $(SWTEMP)/priv/swagger.json $(HTTP_APP)/priv/
	@cp $(SWTEMP)/src/*.erl $(HTTP_APP)/src/swagger
	@rm -fr $(SWTEMP)

swagger-python: config/swagger.yaml $(SWAGGER_CODEGEN_CLI)
	$(SWAGGER_CODEGEN) generate -i $< -l python -o $(SWTEMP)
	@echo "Swagger python tempdir: $(SWTEMP)"
	@cp -r $(SWTEMP)/swagger_client $(PYTHON_TESTS)
	@rm -fr $(SWTEMP)

$(SWAGGER_CODEGEN_CLI):
	curl -fsS --create-dirs -o $@ http://central.maven.org/maven2/io/swagger/swagger-codegen-cli/$(SWAGGER_CODEGEN_CLI_V)/swagger-codegen-cli-$(SWAGGER_CODEGEN_CLI_V).jar

kill:
	@echo "Kill all beam processes only from this directory tree"
	$(shell pkill -9 -f ".*/beam.*-boot `pwd`" || true)

killall:
	@echo "Kill all beam processes from this host"
	@pkill -9 beam || true

clean:
	@./rebar3 clean

multi-build: dev1-build
	@rm -rf _build/dev2 _build/dev3
	@for x in dev2 dev3; do \
		cp -R _build/dev1 _build/$$x; \
		cp config/$$x/sys.config _build/$$x/rel/epoch/releases/$(VER)/sys.config; \
		cp config/$$x/vm.args _build/$$x/rel/epoch/releases/$(VER)/vm.args; \
	done

#
# Build rules
#

.SECONDEXPANSION:

internal-build: $$(KIND)
	@./rebar3 as $(KIND) release

internal-start: $$(KIND)
	@./_build/$(KIND)/$(CORE) start

internal-stop: $$(KIND)
	@./_build/$(KIND)/$(CORE) stop

internal-attach: $$(KIND)
	@./_build/$(KIND)/$(CORE) attach

internal-clean: $$(KIND)
	@rm -rf ./_build/$(KIND)/rel/epoch/data/*
	@rm -rf ./_build/$(KIND)/rel/epoch/blocks/*
	@rm -rf ./_build/$(KIND)/rel/*/log/*



.PHONY: \
	local-build local-start local-stop local-attach \
	prod-build prod-start prod-stop prod-attach \
	multi-build, multi-start, multi-stop, multi-clean \
	dev1-start, dev1-stop, dev1-attach, dev1-clean \
	dev2-start, dev2-stop, dev2-attach, dev2-clean \
	dev3-start, dev3-stop, dev3-attach, dev3-clean \
	dialyzer \
	test \
	kill killall \
	swagger swagger-python
