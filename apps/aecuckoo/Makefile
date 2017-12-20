EXECUTABLES = \
	priv/bin/mean30s-generic \
	priv/bin/mean28s-generic \
	priv/bin/mean16s-generic \
	priv/bin/mean30s-avx2 \
	priv/bin/mean28s-avx2 \
	priv/bin/mean16s-avx2 \
	priv/bin/lean30 \
	priv/bin/lean28 \
	priv/bin/lean16 \
	priv/bin/verify30 \
	priv/bin/verify28 \
	priv/bin/verify16

LIBBLAKE = priv/lib/libblake2b.so

SRC_EXECUTABLES = $(patsubst %,c_src/src/%,$(foreach ex,$(patsubst priv/bin/%,%,$(EXECUTABLES)),$(SRC_EXECUTABLE_NOTDIR_$(ex))))

# Distinguish by self-explanatory name the mean miner binaries
# requiring AVX2 instructions.
SRC_EXECUTABLE_NOTDIR_mean30s-generic = mean30sx1
SRC_EXECUTABLE_NOTDIR_mean28s-generic = mean28sx1
SRC_EXECUTABLE_NOTDIR_mean16s-generic = mean16sx1
SRC_EXECUTABLE_NOTDIR_mean30s-avx2    = mean30sx8
SRC_EXECUTABLE_NOTDIR_mean28s-avx2    = mean28sx8
SRC_EXECUTABLE_NOTDIR_mean16s-avx2    = mean16sx8
SRC_EXECUTABLE_NOTDIR_lean30          = lean30
SRC_EXECUTABLE_NOTDIR_lean28          = lean28
SRC_EXECUTABLE_NOTDIR_lean16          = lean16
SRC_EXECUTABLE_NOTDIR_verify30        = verify30
SRC_EXECUTABLE_NOTDIR_verify28        = verify28
SRC_EXECUTABLE_NOTDIR_verify16        = verify16

SRC_LIBBLAKE = $(patsubst priv/lib/%,c_src/src/%,$(LIBBLAKE))

SRC_ARCH_FLAGS = -m64

REPO = https://github.com/aeternity/cuckoo.git
COMMIT = 441da2df9591918f4b6f6370edc866e0bc31296c

.PHONY: all
all: $(LIBBLAKE) $(EXECUTABLES)
	@: # Silence the `Nothing to be done for 'all'.` message when running `make all`.

.PHONY: clean
clean:
	rm -f $(SRC_EXECUTABLES) $(EXECUTABLES) $(SRC_LIBBLAKE) $(LIBBLAKE)

.PHONY: distclean
distclean:
	rm -rf c_src priv

# Use `.LOW_RESOLUTION_TIME` for preventing unnecessary target
# recomputation caused by inability of `cp -p `to set high resolution
# timestamp on certain filesystems.
.LOW_RESOLUTION_TIME: $(LIBBLAKE)
$(LIBBLAKE): $(SRC_LIBBLAKE) | priv/lib
	cp -p $< $@

priv/lib priv/bin:
	mkdir -p $@

# Do not silence the recursive make call, in order to show compiler
# flags used.
$(SRC_LIBBLAKE): | c_src/.git
	( cd $(@D) && \
		$(MAKE) $(@F) \
			GCC_ARCH_FLAGS=$(SRC_ARCH_FLAGS) \
			GPP_ARCH_FLAGS=$(SRC_ARCH_FLAGS); )

# Do not silence the recursive make call, in order to show compiler
# flags used.
$(SRC_EXECUTABLES): c_src/src/%: | c_src/.git
	( cd $(@D) && \
		$(MAKE) libblake2b.so $* \
			GCC_ARCH_FLAGS=$(SRC_ARCH_FLAGS) \
			GPP_ARCH_FLAGS=$(SRC_ARCH_FLAGS); )

# Clone without checking out, so that recipe interrupted after clone
# does not leave working directory distinct from the expected commit.
c_src/.git:
	git clone -n $(REPO) $(@D)
	( cd $(@D) && git checkout $(COMMIT); )

.SECONDEXPANSION:
# For each target defined after the special target `.SECONDEXPANSION`,
# expand all the prerequisites a second time.

# Use `.LOW_RESOLUTION_TIME` for preventing unnecessary target
# recomputation caused by inability of `cp -p `to set high resolution
# timestamp on certain filesystems.
.LOW_RESOLUTION_TIME: $(EXECUTABLES)
# Rely on secondary expansion to expand the prerequisite a second
# time, in order to determine the name of the source executable from
# the (stem of the) pattern.  E.g. target `priv/bin/mean30s-generic`
# has stem `mean30s-generic` and prerequisite
# `c_src/src/$(SRC_EXECUTABLE_NOTDIR_mean30s-generic)` (because `$` is
# escaped as `$$`) that, during secondary expansion, becomes
# `c_src/src/mean30sx1`.
$(EXECUTABLES): priv/bin/%: c_src/src/$$(SRC_EXECUTABLE_NOTDIR_%) | priv/bin
	cp -p $< $@
