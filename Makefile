CORE = rel/aeternity/bin/aeternity
VER := $(shell cat VERSION)

REBAR ?= ./rebar3

EUNIT_VM_ARGS = $(CURDIR)/config/eunit.vm.args
EUNIT_SYS_CONFIG = $(CURDIR)/config/eunit.sys.config
EUNIT_TEST_FLAGS ?=

CT_TEST_FLAGS ?=
ST_CT_FLAGS = --logdir system_test/logs
ST_CT_DIR = --dir system_test/common
ST_CT_LOCALDIR = --dir system_test/only_local

EQC_EUNIT_TEST_FLAGS ?=
EQC_EUNIT_TESTING_TIME_MULTIPLIER ?= 1
EQC_EUNIT_TEST_FLAGS_FINAL = $(EQC_EUNIT_TEST_FLAGS)

SWAGGER_CODEGEN_CLI_V = 2.4.4
SWAGGER_CODEGEN_CLI = swagger/swagger-codegen-cli-$(SWAGGER_CODEGEN_CLI_V).jar
SWAGGER_CODEGEN = java -jar $(SWAGGER_CODEGEN_CLI)
SWAGGER_ENDPOINTS_SPEC = apps/aeutils/src/endpoints.erl

PACKAGE_SPEC_WIN32 ?= ../ci/appveyor/package.cfg

# Packages from master MUST be pre-releases. Git master version
# usually is higher then the last stable release. However
# packages with newer stable version MUST always have higher version
# than master in Debian/Ubuntu packaging context. The only way to
# achieve this is when master packages are a pre-release (
# pkg-name_version~unique_higher_number ).

# Additionally the same (as in name) package from master for the same
# unreleased/not stable version (i.e. builds) MUST always have higher
# version (i.e. pre-release). Otherwise package managers and repository management
# software complain.
AE_DEB_PKG_VERSION ?= `cat VERSION`
AE_DEB_DCH_REL_NOTE= \
"Release notes are available in /usr/share/doc/aeternity-node/docs/release-notes/RELEASE-NOTES-`cat VERSION`.md"

AE_DEB_PKG_NAME="aeternity-node"
AE_DEB_MAINT_EMAIL="info@aeternity.com"
AE_DEB_MAINT_NAME="Aeternity Team"
DEB_PKG_CHANGELOG_FILE=debian/changelog

all:	local-build

$(SWAGGER_ENDPOINTS_SPEC):
	$(REBAR) swagger_endpoints

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

ifdef EQC_TEST
EQC_EUNIT_TEST_FLAGS_FINAL += --module=$(EQC_TEST)
else
EQC_EUNIT_TEST_FLAGS_FINAL += --app eqc_test
endif

ifdef VERBOSE
CT_TEST_FLAGS += --verbose
unexport VERBOSE
endif

ifdef REPEAT
CT_TEST_FLAGS += --repeat=$(REPEAT)
unexport REPEAT
endif

PYTHON_DIR = py
PYTHON_TESTS = $(PYTHON_DIR)/tests

export AEVM_EXTERNAL_TEST_DIR=aevm_external
export AEVM_EXTERNAL_TEST_VERSION=348b0633f4a6ee3c100368bf0f4fca71394b4d01

HTTP_APP = apps/aehttp
SWTEMP := $(shell mktemp -d 2>/dev/null || mktemp -d -t 'mytmpdir')

console:
	@$(REBAR) as local shell --config config/dev.config --sname aeternity@localhost

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

dialyzer-install: $(SWAGGER_ENDPOINTS_SPEC)
	@$(REBAR) tree
	@$(REBAR) dialyzer -u true -s false

dialyzer: $(SWAGGER_ENDPOINTS_SPEC)
	@$(REBAR) dialyzer

ct-roma: KIND=test
ct-roma: SYSCONFIG=config/test-roma.config
ct-roma: AETERNITY_TESTCONFIG_DB_BACKEND=mnesia
ct-roma: internal-ct

ct-minerva: KIND=test
ct-minerva: SYSCONFIG=config/test-minerva.config
ct-minerva: AETERNITY_TESTCONFIG_DB_BACKEND=mnesia
ct-minerva: internal-ct

ct-fortuna: KIND=test
ct-fortuna: SYSCONFIG=config/test-fortuna.config
ct-fortuna: AETERNITY_TESTCONFIG_DB_BACKEND=mnesia
ct-fortuna: internal-ct

ct-lima: KIND=test
ct-lima: SYSCONFIG=config/test-lima.config
ct-lima: AETERNITY_TESTCONFIG_DB_BACKEND=mnesia
ct-lima: internal-ct

ct-latest: ct-lima

ct-mnesia-leveled: KIND=test
ct-mnesia-leveled: SYSCONFIG=config/test-lima.config
ct-mnesia-leveled: AETERNITY_TESTCONFIG_DB_BACKEND=leveled
ct-mnesia-leveled: internal-ct

ct-mnesia-rocksdb: KIND=test
ct-mnesia-rocksdb: SYSCONFIG=config/test-lima.config
ct-mnesia-rocksdb: AETERNITY_TESTCONFIG_DB_BACKEND=rocksdb
ct-mnesia-rocksdb: internal-ct

REVISION:
	@git rev-parse HEAD > $@

eunit-roma: KIND=test
eunit-roma: internal-build
	@ERL_FLAGS="-args_file $(EUNIT_VM_ARGS) -config $(EUNIT_SYS_CONFIG) -network_id local_roma_testnet" $(REBAR) do eunit $(EUNIT_TEST_FLAGS)

eunit-minerva: KIND=test
eunit-minerva: internal-build
	@ERL_FLAGS="-args_file $(EUNIT_VM_ARGS) -config $(EUNIT_SYS_CONFIG) -network_id local_minerva_testnet" $(REBAR) do eunit $(EUNIT_TEST_FLAGS)

eunit-fortuna: KIND=test
eunit-fortuna: internal-build
	@ERL_FLAGS="-args_file $(EUNIT_VM_ARGS) -config $(EUNIT_SYS_CONFIG) -network_id local_fortuna_testnet" $(REBAR) do eunit $(EUNIT_TEST_FLAGS)

eunit-lima: KIND=test
eunit-lima: internal-build
	@ERL_FLAGS="-args_file $(EUNIT_VM_ARGS) -config $(EUNIT_SYS_CONFIG) -network_id local_lima_testnet" $(REBAR) do eunit $(EUNIT_TEST_FLAGS)

eunit-latest: eunit-lima

all-tests: eunit-lima ct-lima

docker: dockerignore-check
	@docker build -t aeternity/aeternity:local .

dockerignore-check: | .gitignore .dockerignore
	bash -c "diff <(grep '^apps/' $(word 1,$|) | sort) <(grep '^apps/' $(word 2,$|) | sort)"

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

smoke-test: docker smoke-test-run

smoke-test-run: KIND=system_test
smoke-test-run: internal-build
	@$(REBAR) as $(KIND) do ct $(ST_CT_DIR) $(ST_CT_FLAGS) --suite=aest_sync_SUITE,aest_commands_SUITE,aest_peers_SUITE

system-smoke-test-deps:
	$(MAKE) docker
	docker pull "aeternity/aeternity:v1.4.0"

local-system-test: KIND=system_test
local-system-test: internal-build
	@$(REBAR) as $(KIND) do ct $(ST_CT_LOCALDIR) $(ST_CT_FLAGS) $(CT_TEST_FLAGS)

system-test-deps:
	$(MAKE) system-smoke-test-deps
	docker pull "aeternity/aeternity:v2.1.0"
	docker pull "aeternity/aeternity:v2.3.0"
	docker pull "aeternity/aeternity:latest"

system-test: KIND=system_test
system-test: internal-build
	@$(REBAR) as $(KIND) do ct $(ST_CT_DIR) $(ST_CT_FLAGS) $(CT_TEST_FLAGS)

aevm-test: aevm-test-deps
	@$(REBAR) eunit --application=aevm

aevm-test-deps: $(AEVM_EXTERNAL_TEST_DIR)/ethereum_tests
aevm-test-deps:
	$(eval VER=$(shell git -C "$(AEVM_EXTERNAL_TEST_DIR)/ethereum_tests" log -1 --pretty=format:"%H"))
	@if [ "$(VER)" != "$(AEVM_EXTERNAL_TEST_VERSION)" ]; then \
		git -C $(AEVM_EXTERNAL_TEST_DIR)/ethereum_tests checkout $(AEVM_EXTERNAL_TEST_VERSION); \
	fi

$(AEVM_EXTERNAL_TEST_DIR)/ethereum_tests:
	@git clone https://github.com/ethereum/tests.git $(AEVM_EXTERNAL_TEST_DIR)/ethereum_tests

.PHONY: eqc-test
eqc-test: eqc
	env ERL_FLAGS="-eqc_testing_time_multiplier $(EQC_EUNIT_TESTING_TIME_MULTIPLIER)" $(REBAR) as test,eqc eunit $(EQC_EUNIT_TEST_FLAGS_FINAL)

EQC_TEST_REPO = https://github.com/Quviq/epoch-eqc.git
EQC_TEST_VERSION = 0a637d3

.PHONY: eqc
eqc: | eqc/.git
	## TODO Re-fetch repo if version not found in local repo.
	( cd $@ && git reset --quiet --soft $(EQC_TEST_VERSION) && git stash --quiet --all; )

eqc/.git:
	git clone --quiet --no-checkout $(EQC_TEST_REPO) $(@D)

.PHONY: eqc-registration
eqc-registration:
	erl -noinput -run eqc force_registration "$${EQC_REGISTRATION_KEY:?}" -run init stop

.PHONY: eqc-start
eqc-start:
	erl -noinput -run eqc start -run init stop

EQC_LIB_VSN = 1.44.1
EQC_LIB_DOWNLOAD_URL = http://quviq-licencer.com/downloads/eqcR20-$(EQC_LIB_VSN).zip
EQC_LIB_DOWNLOAD_SHA256 = c02a978cb7b7665fee220dda303c86fd517b89ce7fdafc5cc7181eadab26424e
EQC_LIB_ROOT_DIR = "Quviq QuickCheck version $(EQC_LIB_VSN)"

.PHONY: eqc-lib-test eqc-lib-registration eqc-lib-start
eqc-lib-test eqc-lib-registration eqc-lib-start: eqc-lib-%: | eqc-lib/eqc
	( export ERL_LIBS="$(CURDIR)"/$(word 1,$|)/$(EQC_LIB_ROOT_DIR); $(MAKE) eqc-$*; )

eqc-lib/eqc: | eqc-lib/eqc.zip
	unzip $(word 1,$|) -d $@
	ls -d $@/$(EQC_LIB_ROOT_DIR) > /dev/null
	ls $@/$(EQC_LIB_ROOT_DIR)/eqc-$(EQC_LIB_VSN)/ebin/eqc.beam > /dev/null

.SECONDARY: eqc-lib/eqc.zip
eqc-lib/eqc.zip: | eqc-lib/eqc.zip.unchecked
	echo "$(EQC_LIB_DOWNLOAD_SHA256)  $(word 1,$|)" | shasum -a 256 -c -
	mv $(word 1,$|) $@

.INTERMEDIATE: eqc-lib/eqc.zip.unchecked
eqc-lib/eqc.zip.unchecked:
	curl -fsSL --create-dirs -o $@ $(EQC_LIB_DOWNLOAD_URL)

python-env:
	( cd $(PYTHON_DIR) && $(MAKE) env; )

python-uats: swagger
	( cd $(PYTHON_DIR) && $(MAKE) uats; )

python-single-uat: swagger
	( cd $(PYTHON_DIR) && TEST_NAME=$(TEST_NAME) $(MAKE) single-uat; )

python-release-test: swagger
	( cd $(PYTHON_DIR) && WORKDIR="$(WORKDIR)" PACKAGE=$(PACKAGE) VER=$(VER) $(MAKE) release-test; )

python-package-win32-test:
	( cd $(PYTHON_DIR) && WORKDIR="$(WORKDIR)" PACKAGESPECFILE=$(PACKAGE_SPEC_WIN32) $(MAKE) package-win32-test; )

swagger: config/swagger.yaml $(SWAGGER_CODEGEN_CLI) $(SWAGGER_ENDPOINTS_SPEC)
	@$(SWAGGER_CODEGEN) generate -i $< -l erlang-server -o $(SWTEMP)
	@echo "Swagger tempdir: $(SWTEMP)"
	@( mkdir -p $(HTTP_APP)/priv && cp $(SWTEMP)/priv/swagger.json $(HTTP_APP)/priv/; )
	@( cd $(HTTP_APP) && $(MAKE) updateswagger; )
	@rm -fr $(SWTEMP)
	@$(SWAGGER_CODEGEN) generate -i $< -l python -o $(SWTEMP) --import-mappings GAObject="from swagger_client.models.hack_ga_object import GAObject"
	@echo "Swagger python tempdir: $(SWTEMP)"
	@cp -r $(SWTEMP)/swagger_client $(PYTHON_TESTS)
	@cp $(PYTHON_DIR)/hack_ga_object.py $(PYTHON_TESTS)/swagger_client/models
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
	@$(REBAR) clean
	@-rm REVISION
	@-rm $(SWAGGER_ENDPOINTS_SPEC)
	( cd $(HTTP_APP) && $(MAKE) clean; )
	@$(MAKE) multi-distclean
	@$(MAKE) eqc-clean
	@rm -rf _build/system_test+test _build/system_test _build/test _build/prod _build/local
	@rm -rf _build/default/plugins
	@rm -rf $$(ls -d _build/default/lib/* | grep -v '[^_]rocksdb') ## Dependency `rocksdb` takes long to build.

.PHONY: eqc-clean
eqc-clean:
	rm -rf .eqc-info current_counterexample.eqc
	rm -rf _build/test+eqc

## Do not delete `eqc`.
distclean: clean
	( cd otp_patches && $(MAKE) distclean; )
	( cd $(HTTP_APP) && $(MAKE) distclean; )
	@rm -rf _build/

multi-build: dev1-build
	@$(MAKE) dev2-distclean
	@$(MAKE) dev3-distclean
	@for x in dev2 dev3; do \
		cp -R _build/dev1 _build/$$x; \
		cp config/$$x/sys.config _build/$$x/rel/aeternity/releases/$(VER)/sys.config; \
		cp config/$$x/vm.args _build/$$x/rel/aeternity/releases/$(VER)/vm.args; \
	done

#
# Build rules
#

internal-compile-deps:
	@$(REBAR) as $(KIND) compile -d

internal-package: REVISION internal-compile-deps $(SWAGGER_ENDPOINTS_SPEC)
	@$(REBAR) as $(KIND) tar

internal-build: REVISION internal-compile-deps $(SWAGGER_ENDPOINTS_SPEC)
	@$(REBAR) as $(KIND) release

internal-start:
	@./_build/$(KIND)/$(CORE) start

internal-stop:
	@./_build/$(KIND)/$(CORE) stop

internal-attach:
	@./_build/$(KIND)/$(CORE) attach

internal-clean:
	@rm -rf ./_build/$(KIND)/rel/aeternity/data/mnesia
	@rm -rf ./_build/$(KIND)/rel/*/log/*

internal-distclean:
	@rm -rf ./_build/$(KIND)

internal-ct: internal-build
	@NODE_PROCESSES="$$(ps -fea | grep bin/aeternity | grep -v grep)"; \
	if [ $$(printf "%b" "$${NODE_PROCESSES}" | wc -l) -gt 0 ] ; then \
		(printf "%b\n%b\n" "Failed testing: another node is already running" "$${NODE_PROCESSES}" >&2; exit 1);\
	else \
		AETERNITY_TESTCONFIG_DB_BACKEND=$(AETERNITY_TESTCONFIG_DB_BACKEND) \
			$(REBAR) ct $(CT_TEST_FLAGS) --sys_config $(SYSCONFIG); \
	fi

$(DEB_PKG_CHANGELOG_FILE):
	@export DEBEMAIL=$(AE_DEB_MAINT_EMAIL); \
	export DEBFULLNAME=$(AE_DEB_MAINT_NAME) ; \
	dch --create --package=$(AE_DEB_PKG_NAME) -v $(AE_DEB_PKG_VERSION) $(AE_DEB_DCH_REL_NOTE); \
	dch -r $(AE_DEB_DCH_REL_NOTE)

prod-deb-package: $(DEB_PKG_CHANGELOG_FILE)
	debuild --preserve-envvar DEB_SKIP_DH_AUTO_CLEAN -b -uc -us

.PHONY: \
	all console \
	local-build local-start local-stop local-attach \
	prod-build prod-start prod-stop prod-attach prod-package prod-compile-deps \
	multi-build multi-start multi-stop multi-clean multi-distclean \
	dev1-start dev1-stop dev1-attach dev1-clean dev1-distclean \
	dev2-start dev2-stop dev2-attach dev2-clean dev2-distclean \
	dev3-start dev3-stop dev3-attach dev3-clean dev3-distclean \
	internal-start internal-stop internal-attach internal-clean internal-compile-deps internal-ct \
	dialyzer \
	docker docker-clean dockerignore-check \
	test smoke-test smoke-test-run system-test aevm-test-deps \
	ct-latest ct-roma ct-minerva ct-fortuna ct-lima ct-mnesia-leveled ct-mnesia-rocksdb \
	eunit-latest eunit-roma eunit-minerva eunit-fortuna eunit-lima\
	system-smoke-test-deps system-test-deps \
	kill killall \
	clean distclean \
	swagger swagger-docs swagger-check swagger-version-check \
	rebar-lock-check \
	python-env python-ws-test python-uats python-single-uat python-release-test python-package-win32-test \
	REVISION \
	prod-deb-package
