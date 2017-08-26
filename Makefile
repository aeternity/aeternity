CORE = rel/epoch/bin/epoch


local-build: KIND=local
local-build: build

local-start: KIND=local
local-start: start

local-stop: KIND=local
local-stop: stop

local-attach: KIND=local
local-attach: attach

prod-build: KIND=prod
prod-build: build

prod-start: KIND=prod
prod-start: start

prod-stop: KIND=prod
prod-stop: stop

prod-attach: KIND=prod
prod-attach: attach


dialyzer:
	@./rebar3 dialyzer

test:
	@./rebar3 do eunit,ct


kill:
	@echo "Kill all beam processes only from this directory tree"
	$(shell pkill -9 -f ".*/beam.*-boot `pwd`" || true)

killall:
	@echo "Kill all beam processes from this host"
	@pkill -9 beam || true


#
# Build rules
#

.SECONDEXPANSION:

build: $$(KIND)
	@./rebar3 as $(KIND) release

start: $$(KIND)
	@./_build/$(KIND)/$(CORE) start

stop: $$(KIND)
	@./_build/$(KIND)/$(CORE) stop

attach: $$(KIND)
	@./_build/$(KIND)/$(CORE) attach


.PHONY: \
	local-build local-start local-stop local-attach \
	prod-build prod-start prod-stop prod-attach \
	dialyzer \
	test \
	kill killall \
