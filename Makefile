CORE = rel/epoch/bin/epoch
VER := $(shell cat VERSION)

EUNIT_VM_ARGS = $(CURDIR)/config/eunit.vm.args
EUNIT_TEST_FLAGS ?=

CT_TEST_FLAGS ?=
ST_CT_FLAGS = --dir system_test --logdir system_test/logs

null  :=
space := $(null) # space
comma := ,

comma-separate = $(subst ${space},${comma},$(strip $1))
space-separate = $(subst ${comma},${space},$(strip $1))

ifdef SUITE
CT_TEST_FLAGS += --suite=$(call comma-separate,$(foreach suite,$(call space-separate,${SUITE}),${suite}_SUITE))
unexport SUITE
endif

ifdef GROUP
CT_TEST_FLAGS += --group=$(GROUP)
unexport GROUP
endif

ifdef TEST
CT_TEST_FLAGS += --case=$(TEST)
EUNIT_TEST_FLAGS += --module=$(TEST)
unexport TEST
endif

ifdef VERBOSE
CT_TEST_FLAGS += --verbose
unexport VERBOSE
endif

PYTHON_DIR = py
PYTHON_TESTS = $(PYTHON_DIR)/tests
PIP = $(PYTHON_BIN)/pip

export AEVM_EXTERNAL_TEST_DIR=aevm_external
export AEVM_EXTERNAL_TEST_VERSION=348b0633f4a6ee3c100368bf0f4fca71394b4d01

HTTP_APP = apps/aehttp
SWTEMP := $(shell mktemp -d 2>/dev/null || mktemp -d -t 'mytmpdir')

all:	local-build

console:
	@./rebar3 shell --config config/dev.config --sname epoch

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

prod-compile-deps: KIND=prod
prod-compile-deps: internal-compile-deps

prod-build: KIND=prod
prod-build: internal-build

prod-start: KIND=prod
prod-start: internal-start

prod-stop: KIND=prod
prod-stop: internal-stop

prod-attach: KIND=prod
prod-attach: internal-attach

prod-clean: KIND=prod
prod-clean: internal-clean

multi-start:
	@$(MAKE) dev1-start
	@$(MAKE) dev2-start
	@$(MAKE) dev3-start

multi-stop:
	@$(MAKE) dev1-stop
	@$(MAKE) dev2-stop
	@$(MAKE) dev3-stop

multi-clean:
	@$(MAKE) dev1-clean
	@$(MAKE) dev2-clean
	@$(MAKE) dev3-clean

multi-distclean:
	@$(MAKE) dev1-distclean
	@$(MAKE) dev2-distclean
	@$(MAKE) dev3-distclean

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

dev1-distclean: KIND=dev1
dev1-distclean: internal-distclean

dev2-start: KIND=dev2
dev2-start: internal-start

dev2-stop: KIND=dev2
dev2-stop: internal-stop

dev2-attach: KIND=dev2
dev2-attach: internal-attach

dev2-clean: KIND=dev2
dev2-clean: internal-clean

dev2-distclean: KIND=dev2
dev2-distclean: internal-distclean

dev3-start: KIND=dev3
dev3-start: internal-start

dev3-stop: KIND=dev3
dev3-stop: internal-stop

dev3-attach: KIND=dev3
dev3-attach: internal-attach

dev3-clean: KIND=dev3
dev3-clean: internal-clean

dev3-distclean: KIND=dev3
dev3-distclean: internal-distclean

dialyzer-install:
	@./rebar3 tree
	@./rebar3 dialyzer -u true -s false

dialyzer:
	@./rebar3 dialyzer

test:
	@EPOCH_PROCESSES="$$(ps -fea | grep bin/epoch | grep -v grep)"; \
	if [ $$(printf "%b" "$${EPOCH_PROCESSES}" | wc -l) -gt 0 ] ; then \
		(printf "%b\n%b\n" "Failed testing: another Epoch node is already running" "$${EPOCH_PROCESSES}" >&2; exit 1);\
	else \
		./rebar3 as test do release, ct $(CT_TEST_FLAGS) --sys_config config/test.config; \
	fi

eunit:
	@ERL_FLAGS="-args_file $(EUNIT_VM_ARGS)" ./rebar3 do eunit $(EUNIT_TEST_FLAGS)

all-tests: eunit test

docker:
	@docker build . -t aeternity/epoch:local

ST_DOCKER_FILTER=--filter label=epoch_system_test=true

docker-clean:
	$(eval RUNNING=$(shell docker container ls $(ST_DOCKER_FILTER) --format '{{.ID}}'))
	@if [ -n "$(RUNNING)" ]; then \
		echo "Stopping containers..."; \
		docker container stop $(RUNNING); \
	fi
	@echo "Deleting containers..."
	@docker container prune -f $(ST_DOCKER_FILTER)
	@echo "Deleting images..."
	@docker image prune -a -f $(ST_DOCKER_FILTER)

smoke-test: system-test-deps docker smoke-test-run

smoke-test-run:
	@./rebar3 as system_test do ct $(ST_CT_FLAGS) --suite=aest_sync_SUITE,aest_commands_SUITE,aest_peers_SUITE

system-test:
	@./rebar3 as system_test do ct $(ST_CT_FLAGS) $(CT_TEST_FLAGS)

system-test-deps:
#	docker pull aeternity/epoch:latest

TESTNET_DB_BACKUP_FILE = mnesia_18.196.250.42_uat_db_backup_1524700922.gz
TESTNET_DB_BACKUP_BASE_URL = https://9305-99802036-gh.circle-artifacts.com/0/tmp/chain_snapshots/18.196.250.42/tmp

aevm-test: aevm-test-deps
	@./rebar3 eunit --application=aevm

aevm-test-deps: $(AEVM_EXTERNAL_TEST_DIR)/ethereum_tests
aevm-test-deps:
	$(eval VER=$(shell git -C "$(AEVM_EXTERNAL_TEST_DIR)/ethereum_tests" log -1 --pretty=format:"%H"))
	@if [ "$(VER)" != "$(AEVM_EXTERNAL_TEST_VERSION)" ]; then \
		git -C $(AEVM_EXTERNAL_TEST_DIR)/ethereum_tests checkout $(AEVM_EXTERNAL_TEST_VERSION); \
	fi

$(AEVM_EXTERNAL_TEST_DIR)/ethereum_tests:
	@git clone https://github.com/ethereum/tests.git $(AEVM_EXTERNAL_TEST_DIR)/ethereum_tests


python-env:
	( cd $(PYTHON_DIR) && $(MAKE) env; )

python-ws-test: swagger
	( cd $(PYTHON_DIR) && $(MAKE) websocket-test; )

python-uats: swagger
	( cd $(PYTHON_DIR) && $(MAKE) uats; )

python-single-uat: swagger
	( cd $(PYTHON_DIR) && TEST_NAME=$(TEST_NAME) $(MAKE) single-uat; )

python-release-test: swagger
	( cd $(PYTHON_DIR) && WORKDIR="$(WORKDIR)" TARBALL=$(TARBALL) VER=$(VER) $(MAKE) release-test; )

SWAGGER_CODEGEN_CLI_V = 2.3.1
SWAGGER_CODEGEN_CLI = swagger/swagger-codegen-cli-$(SWAGGER_CODEGEN_CLI_V).jar
SWAGGER_CODEGEN = java -jar $(SWAGGER_CODEGEN_CLI)

swagger: config/swagger.yaml $(SWAGGER_CODEGEN_CLI)
	@$(SWAGGER_CODEGEN) generate -i $< -l erlang-server -o $(SWTEMP)
	@echo "Swagger tempdir: $(SWTEMP)"
	@( mkdir -p $(HTTP_APP)/priv && cp $(SWTEMP)/priv/swagger.json $(HTTP_APP)/priv/; )
	@( cd $(HTTP_APP) && $(MAKE) updateswagger; )
	@rm -fr $(SWTEMP)
	@./rebar3 swagger_endpoints
	@$(SWAGGER_CODEGEN) generate -i $< -l python -o $(SWTEMP)
	@echo "Swagger python tempdir: $(SWTEMP)"
	@cp -r $(SWTEMP)/swagger_client $(PYTHON_TESTS)
	@rm -fr $(SWTEMP)

swagger-docs:
	(cd ./apps/aehttp && $(MAKE) swagger-docs);

swagger-version-check:
	@( cd $(PYTHON_DIR) && \
		VERSION="$(CURDIR)/VERSION" \
		SWAGGER_YAML=$(CURDIR)/config/swagger.yaml \
		SWAGGER_JSON=$(CURDIR)/apps/aehttp/priv/swagger.json \
		$(MAKE) swagger-version-check )

swagger-check: swagger-version-check
	./swagger/check \
		"$(CURDIR)/config/swagger.yaml" \
		"swagger" \
		"$(CURDIR)/apps/aehttp/priv/swagger.json" \
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
	( cd apps/aesophia/test/contracts && $(MAKE) clean; )
	( cd $(HTTP_APP) && $(MAKE) clean; )
	@$(MAKE) multi-distclean
	@rm -rf _build/system_test+test _build/system_test _build/test _build/prod _build/local
	@rm -rf _build/default/plugins
	@rm -rf $$(ls -d _build/default/lib/* | grep -v '[^_]rocksdb') ## Dependency `rocksdb` takes long to build.

distclean: clean
	( cd apps/aecuckoo && $(MAKE) distclean; )
	( cd otp_patches && $(MAKE) distclean; )
	( cd $(HTTP_APP) && $(MAKE) distclean; )
	@rm -rf _build/

multi-build: dev1-build
	@$(MAKE) dev2-distclean
	@$(MAKE) dev3-distclean
	@for x in dev2 dev3; do \
		cp -R _build/dev1 _build/$$x; \
		cp config/$$x/sys.config _build/$$x/rel/epoch/releases/$(VER)/sys.config; \
		cp config/$$x/vm.args _build/$$x/rel/epoch/releases/$(VER)/vm.args; \
	done

#
# Build rules
#

.SECONDEXPANSION:

internal-compile-deps: $$(KIND)
	@./rebar3 as $(KIND) compile --deps-only

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
	@rm -rf ./_build/$(KIND)/rel/epoch/data/mnesia
	@rm -rf ./_build/$(KIND)/rel/*/log/*

internal-distclean: $$(KIND)
	@rm -rf ./_build/$(KIND)


.PHONY: \
	all console \
	local-build local-start local-stop local-attach \
	prod-build prod-start prod-stop prod-attach prod-package prod-compile-deps \
	multi-build multi-start multi-stop multi-clean multi-distclean \
	dev1-start dev1-stop dev1-attach dev1-clean dev1-distclean \
	dev2-start dev2-stop dev2-attach dev2-clean dev2-distclean \
	dev3-start dev3-stop dev3-attach dev3-clean dev3-distclean \
	internal-start internal-stop internal-attach internal-clean internal-compile-deps \
	dialyzer \
	docker docker-clean \
	test smoke-test smoke-test-run system-test system-test-deps aevm-test-deps\
	kill killall \
	clean distclean \
	swagger swagger-docs swagger-check swagger-version-check \
	rebar-lock-check
