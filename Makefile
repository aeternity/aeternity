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
PYTHON_TESTS = $(PYTHON_DIR)/tests
PIP = $(PYTHON_BIN)/pip

export AEVM_EXTERNAL_TEST_DIR=aevm_external

HTTP_APP = apps/aehttp
SWTEMP := $(shell mktemp -d 2>/dev/null || mktemp -d -t 'mytmpdir')

all:	local-build

console:
	@./rebar3 shell --config config/sys.config --sname epoch

local-build: KIND=local
local-build: internal-build

local-start: KIND=local
local-start: internal-start

local-stop: KIND=local
local-stop: internal-stop

local-attach: KIND=local
local-attach: internal-attach

prod-package: KIND=prod
prod-package: internal-package

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

dialyzer-install:
	@./rebar3 tree
	@./rebar3 dialyzer -u true -s false

dialyzer:
	@./rebar3 dialyzer

test:
	@./rebar3 as test do release, ct $(CT_TEST_FLAGS) --sys_config config/test.config

eunit:
	@./rebar3 do eunit $(EUNIT_TEST_FLAGS)

all-tests: eunit test

aevm-test: aevm-test-deps
	@./rebar3 eunit --application=aevm

aevm-test-deps: $(AEVM_EXTERNAL_TEST_DIR)/ethereum_tests

$(AEVM_EXTERNAL_TEST_DIR)/ethereum_tests:
	@git clone https://github.com/ethereum/tests.git $(AEVM_EXTERNAL_TEST_DIR)/ethereum_tests


venv-present:
	@virtualenv -q $(PYTHON_DIR)

python-env: venv-present
	( cd $(PYTHON_DIR) && $(MAKE) env; )

python-ws-test:
	( cd $(PYTHON_DIR) && $(MAKE) websocket-test; )

python-uats:
	( cd $(PYTHON_DIR) && $(MAKE) uats; )

python-single-uat:
	( cd $(PYTHON_DIR) && TEST_NAME=$(TEST_NAME) $(MAKE) single-uat; )

python-release-test:
	( cd $(PYTHON_DIR) && WORKDIR="$(WORKDIR)" TARBALL=$(TARBALL) VER=$(VER) $(MAKE) release-test; )

SWAGGER_CODEGEN_CLI_V = 2.3.1
SWAGGER_CODEGEN_CLI = swagger/swagger-codegen-cli-$(SWAGGER_CODEGEN_CLI_V).jar
SWAGGER_CODEGEN = java -jar $(SWAGGER_CODEGEN_CLI)

swagger: config/swagger.yaml $(SWAGGER_CODEGEN_CLI)
	@$(SWAGGER_CODEGEN) generate -i $< -l erlang-server -o $(SWTEMP)
	@echo "Swagger tempdir: $(SWTEMP)"
	@( mkdir -p $(HTTP_APP)/priv && cp $(SWTEMP)/priv/swagger.json $(HTTP_APP)/priv/; )
	@( cd $(HTTP_APP) && $(MAKE) updateswagger; )
	@( mkdir -p $(HTTP_APP)/src/swagger && cp $(SWTEMP)/src/*.erl $(HTTP_APP)/src/swagger; )
	@rm -fr $(SWTEMP)
	@./rebar3 swagger_endpoints
	@$(SWAGGER_CODEGEN) generate -i $< -l python -o $(SWTEMP)
	@echo "Swagger python tempdir: $(SWTEMP)"
	@cp -r $(SWTEMP)/swagger_client $(PYTHON_TESTS)
	@rm -fr $(SWTEMP)

swagger-docs:
	(cd ./apps/aehttp && $(MAKE) swagger-docs);

swagger-check:
	./swagger/check \
		"$(CURDIR)/config/swagger.yaml" \
		"swagger" \
		"$(CURDIR)/apps/aehttp/priv/swagger.json" \
		"$(CURDIR)/apps/aehttp/src/swagger" \
		"$(CURDIR)/py/tests/swagger_client"

$(SWAGGER_CODEGEN_CLI):
	curl -fsS --create-dirs -o $@ http://central.maven.org/maven2/io/swagger/swagger-codegen-cli/$(SWAGGER_CODEGEN_CLI_V)/swagger-codegen-cli-$(SWAGGER_CODEGEN_CLI_V).jar

rebar-lock-check:
	./scripts/rebar_lock_check \
		"$(CURDIR)/rebar3" \
		"$(CURDIR)"

kill:
	@echo "Kill all beam processes only from this directory tree"
	$(shell pkill -9 -f ".*/beam.*-boot `pwd`" || true)

killall:
	@echo "Kill all beam processes from this host"
	@pkill -9 beam || true

clean:
	@./rebar3 clean
	( cd apps/aering/test/contracts && $(MAKE) clean; )
	( cd $(HTTP_APP) && $(MAKE) clean; )
	@rm -rf _build/

distclean: clean
	( cd apps/aecuckoo && $(MAKE) distclean; )
	( cd otp_patches && $(MAKE) distclean; )
	( cd $(HTTP_APP) && $(MAKE) distclean; )

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

internal-package: $$(KIND)
	@./rebar3 as $(KIND) tar

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
	all console \
	local-build local-start local-stop local-attach \
	prod-build prod-start prod-stop prod-attach prod-package \
	multi-build, multi-start, multi-stop, multi-clean \
	dev1-start, dev1-stop, dev1-attach, dev1-clean \
	dev2-start, dev2-stop, dev2-attach, dev2-clean \
	dev3-start, dev3-stop, dev3-attach, dev3-clean \
	dialyzer \
	test aevm-test-deps\
	kill killall \
	clean distclean \
	swagger swagger-docs swagger-check \
	rebar-lock-check
