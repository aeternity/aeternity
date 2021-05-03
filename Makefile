CORE = rel/aeternity/bin/aeternity

REBAR ?= ./rebar3

PROTOCOLS = roma minerva fortuna lima iris
CT_TARGETS = $(patsubst %,ct-%,$(PROTOCOLS))
LATEST_PROTOCOL = $(lastword $(PROTOCOLS))

DB_BACKENDS = mnesia-rocksdb mnesia-leveled
CT_DB_TARGETS = $(patsubst %,ct-%,$(DB_BACKENDS))

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

SWAGGER_ENDPOINTS_SPEC = apps/aeutils/src/endpoints.erl
OAS_ENDPOINTS_SPEC = apps/aeutils/src/oas_endpoints.erl
OAS_YAML = apps/aehttp/priv/oas3.yaml

CONTRACT_FILE = apps/aehyperchains/src/contracts/SimpleElection.aes
#CONTRACT_FILE = SimpleElection.aes
CONTRACT_OBJECT = data/aehyperchains/StakingContract.json

PACKAGE_SPEC_WIN32 ?= ../.circleci/windows/package.cfg

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
AE_DEB_PKG_NAME="aeternity-node"
AE_DEB_MAINT_EMAIL="info@aeternity.com"
AE_DEB_MAINT_NAME="Aeternity Team"
DEB_PKG_CHANGELOG_FILE=debian/changelog

all:	local-build


endpoints: VERSION
	$(REBAR) swagger_endpoints
	$(REBAR) swagger_endpoints --file=$(OAS_YAML) --out=$(OAS_ENDPOINTS_SPEC) 

null  :=
space := $(null) # space
comma := ,

comma-separate = $(subst ${space},${comma},$(strip $1))
space-separate = $(subst ${comma},${space},$(strip $1))

PLANTUML_V = 1.2020.8
PLANTUML_JAR = docs/.tools/plantuml-$(PLANTUML_V).jar
PLANTUML = java -jar $(PLANTUML_JAR)

uml-files := $(shell find docs -type f -name "*.puml")
uml-svg-files := $(uml-files:.puml=.svg)

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

export AEVM_EXTERNAL_TEST_DIR=aevm_external
export AEVM_EXTERNAL_TEST_VERSION=348b0633f4a6ee3c100368bf0f4fca71394b4d01

console: VERSION REVISION endpoints 
	@$(REBAR) as local shell --config config/dev.config --sname aeternity@localhost

hyperchains-console: $(SWAGGER_ENDPOINTS_SPEC)
	@$(REBAR) as local shell --config config/hyperchains-dev.config --sname aeternity@localhost

test-build: KIND=test
test-build: internal-build aestratum_client_build

AESTRATUM_CLIENT_DIR = _build/$(KIND)/lib/aestratum_client
aestratum_client_build:
	touch $(AESTRATUM_CLIENT_DIR)/.build_lock
	cd $(AESTRATUM_CLIENT_DIR); \
	if ! cmp .git/HEAD .build_lock; then \
	  	mkdir -p _build/default; \
	  	rm -rf _build/default; \
		../../../../$(REBAR) as test release; \
		cp .git/HEAD .build_lock; \
	fi

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

dialyzer-install: endpoints
	@$(REBAR) tree
	@$(REBAR) dialyzer -u true -s false

dialyzer: endpoints 
	@$(REBAR) dialyzer

edoc: VERSION
	@$(REBAR) edoc

$(CT_TARGETS):
	@KIND=test \
	SYSCONFIG=config/test-$(patsubst ct-%,%,$@).config \
	AETERNITY_TESTCONFIG_DB_BACKEND=mnesia \
	$(MAKE) internal-ct

ct-latest: ct-$(LATEST_PROTOCOL)
ct-latest-no-aci:
	$(MAKE) SOPHIA_NO_ACI=true CT_TEST_FLAGS=--suite=apps/aehttp/test/aehttp_contracts_SUITE,apps/aehttp/test/aehttp_coin_toss_SUITE ct-latest

$(CT_DB_TARGETS):
	KIND=test \
	SYSCONFIG=config/test-$(LATEST_PROTOCOL).config \
	AETERNITY_TESTCONFIG_DB_BACKEND=$(patsubst ct-mnesia-%,%,$@) \
	$(MAKE) internal-ct

REVISION:
	@git rev-parse HEAD > $@ || echo "unknown" > $@

VERSION:
	@git describe --tags | \
		sed -E "s/^v(.*)\-([0-9]+)\-g([a-f0-9]+)$$/\1+\2.\3/" | \
		sed -E "s/^v([0-9]+)\.([0-9]+)\.([0-9]+)$$/\1.\2.\3/" > $@

eunit-%: KIND=test
eunit-%: internal-build
	ERL_FLAGS="-args_file $(EUNIT_VM_ARGS) -config $(EUNIT_SYS_CONFIG) -network_id local_$*_testnet" $(REBAR) do eunit $(EUNIT_TEST_FLAGS)

eunit-latest: eunit-$(LATEST_PROTOCOL)

all-tests: eunit-$(LATEST_PROTOCOL) ct-$(LATEST_PROTOCOL)

docker: dockerignore-check
	@docker pull aeternity/builder:otp21
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
	@$(REBAR) as $(KIND),test do upgrade, ct $(ST_CT_DIR) $(ST_CT_FLAGS) --suite=aest_sync_SUITE,aest_commands_SUITE,aest_peers_SUITE

system-smoke-test-deps:
	$(MAKE) docker
	docker pull "aeternity/aeternity:v1.4.0"

local-system-test: KIND=system_test
local-system-test: internal-build
	@$(REBAR) as $(KIND) do ct $(ST_CT_LOCALDIR) $(ST_CT_FLAGS) $(CT_TEST_FLAGS)

system-test-deps:
	$(MAKE) system-smoke-test-deps
	docker pull "aeternity/aeternity:v2.3.0"
	docker pull "aeternity/aeternity:v4.0.0"
	docker pull "aeternity/aeternity:v4.2.0"
	docker pull "aeternity/aeternity:latest"

system-test: KIND=system_test
system-test: internal-build
	@$(REBAR) as $(KIND) do ct $(ST_CT_DIR) $(ST_CT_FLAGS) $(CT_TEST_FLAGS)

aevm-test: VERSION aevm-test-deps
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
EQC_TEST_VERSION = 0cf1674

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

kill:
	@echo "Kill all beam processes only from this directory tree"
	$(shell pkill -9 -f ".*/beam.*-boot `pwd`" || true)

killall:
	@echo "Kill all beam processes from this host"
	@pkill -9 beam || true

clean:
	@$(REBAR) clean
	@-rm REVISION
	@-rm VERSION
	@-rm $(SWAGGER_ENDPOINTS_SPEC)
	@-rm $(OAS_ENDPOINTS_SPEC)
	@$(MAKE) multi-distclean
	@$(MAKE) eqc-clean
	@rm -rf _build/system_test+test _build/system_test _build/test _build/prod _build/local
	@rm -rf _build/default/plugins
	@rm -rf $$(ls -d _build/default/lib/* | grep -v '[^_]rocksdb') ## Dependency `rocksdb` takes long to build.
	@rm -rf ${uml-svg-files}

.PHONY: eqc-clean
eqc-clean:
	rm -rf .eqc-info current_counterexample.eqc
	rm -rf _build/test+eqc

## Do not delete `eqc`.
distclean: clean
	( cd otp_patches && $(MAKE) distclean; )
	@rm -rf _build/

multi-build: VERSION dev1-build
	$(eval VER=$(shell cat VERSION))
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

internal-compile-deps: hc-compile-staking-contract
	@$(REBAR) as $(KIND) compile -d

internal-package: VERSION REVISION internal-compile-deps endpoints
	@$(REBAR) as $(KIND) tar

internal-build: VERSION REVISION internal-compile-deps endpoints
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

internal-ct: test-build
	@NODE_PROCESSES="$$(ps -fea | grep bin/aeternity | grep -v grep)"; \
	if [ $$(printf "%b" "$${NODE_PROCESSES}" | wc -l) -gt 0 ] ; then \
		(printf "%b\n%b\n" "Failed testing: another node is already running" "$${NODE_PROCESSES}" >&2; exit 1);\
	else \
		AETERNITY_TESTCONFIG_DB_BACKEND=$(AETERNITY_TESTCONFIG_DB_BACKEND) \
			$(REBAR) ct $(CT_TEST_FLAGS) --sys_config $(SYSCONFIG); \
	fi

$(DEB_PKG_CHANGELOG_FILE): VERSION REVISION
	export DEBEMAIL=$(AE_DEB_MAINT_EMAIL); \
	export DEBFULLNAME=$(AE_DEB_MAINT_NAME); \
	AE_DEB_PKG_VERSION=`cat VERSION | sed -E s/\-/\~/`-1; \
	AE_DEB_DCH_REL_NOTE="Release notes are available in /usr/share/doc/aeternity-node/docs/release-notes/RELEASE-NOTES-`cat VERSION`.md"; \
	dch --create --package=$(AE_DEB_PKG_NAME) -v $$AE_DEB_PKG_VERSION $$AE_DEB_DCH_REL_NOTE; \
	dch -r $$AE_DEB_DCH_REL_NOTE

prod-deb-package: $(DEB_PKG_CHANGELOG_FILE)
	debuild -e DEB_SKIP_DH_AUTO_CLEAN -e ERLANG_ROCKSDB_OPTS -e ERLANG_ROCKSDB_BUILDOPTS -b -uc -us

$(PLANTUML_JAR):
	curl -fsS --create-dirs -o $@ https://netcologne.dl.sourceforge.net/project/plantuml/${PLANTUML_V}/plantuml.${PLANTUML_V}.jar

build-uml: ${PLANTUML_JAR} ${uml-svg-files}

%.svg: %.puml
	${PLANTUML} -tsvg $<

# Convenience target to test/force re-generation of the Fate op module
regen-fate:
	make -C apps/aefate clean
	${REBAR} compile

test-arch-os-dependencies: KIND=test
test-arch-os-dependencies:
	make ct-latest SUITE=apps/aecontract/test/aecontract GROUP=sophia TEST=sophia_crypto

hc-compile-staking-contract:
	./rebar3 aesophia -s v4.3.1 -c $(CONTRACT_FILE) -o $(CONTRACT_OBJECT)

# TODO: Verify release packages
hc-verify-staking-contract:
	./rebar3 aesophia -s v4.3.1 -c $(CONTRACT_FILE) -o $(CONTRACT_OBJECT) -v

.PHONY: \
	all console hyperchains-console \
	test-build \
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
	ct-% ct-latest ct-mnesia-% \
	eunit-% eunit-latest \
	system-smoke-test-deps system-test-deps \
	test-arch-os-dependencies \
	kill killall \
	clean distclean \
	build-uml \
	REVISION \
	VERSION \
	prod-deb-package \
	regen-fate \
	hc-compile-staking-contract hc-verify-staking-contract
