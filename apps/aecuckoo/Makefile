EXECUTABLES = \
	mean29-generic \
	mean29-avx2 \
	lean29-generic \
	lean29-avx2 \
	mean15-generic \
	lean15-generic

PRIVEXECS = $(addprefix $(PRIV)/, $(EXECUTABLES))

PRIV = priv/bin
CUCKOO = c_src/src/cuckoo

HDRS=$(CUCKOO)/cuckoo.h $(CUCKOO)/../crypto/siphash.h

# Flags from upstream makefile
OPT ?= -O3

GPP_ARCH_FLAGS ?= -m64 -x c++

# -Wno-deprecated-declarations shuts up Apple OSX clang
FLAGS ?= -Wall -Wno-format -Wno-deprecated-declarations -D_POSIX_C_SOURCE=200112L $(OPT) -DPREFETCH -I. $(CPPFLAGS) -pthread
GPP ?= g++ $(GPP_ARCH_FLAGS) -std=c++11 $(FLAGS)
BLAKE_2B_SRC ?= ../crypto/blake2b-ref.c
NVCC ?= nvcc -std=c++11

# end Flags from upstream

REPO = https://github.com/aeternity/cuckoo.git
COMMIT = 3ae6195a67d9dcc33b1d82fd38f9bb82f2d29ee8

.PHONY: all
all: $(EXECUTABLES)
	@: # Silence the `Nothing to be done for 'all'.` message when running `make all`.

.PHONY: clean
clean:
	@if [ -d $(PRIV) ]; then (cd $(PRIV); rm -f $(EXECUTABLES)); fi
	@if [ -d $(CUCKOO) ]; then (cd $(CUCKOO); rm -f $(EXECUTABLES)); fi

.PHONY: distclean
distclean:
	rm -rf c_src priv

# We want rules also for cuda29/lcuda29
EXECUTABLES += lcuda29 cuda29

.SECONDEXPANSION:
.PHONY: $(EXECUTABLES)
$(EXECUTABLES): | c_src/.git $(PRIV)
$(EXECUTABLES): git-version $(CUCKOO)/$$@ $(PRIV)/$$@

# So we need check out the right commit or cleanup if
# pointing to the wrong/old/dirty thing.
.PHONY: git-version
git-version:
ifneq ($(strip $(shell if [ -d c_src ]; then (cd c_src; git rev-parse HEAD); fi)),$(COMMIT))
	(cd c_src; git fetch; git checkout --force $(COMMIT))
else
ifneq ($(strip $(shell cd c_src && git diff-index $(COMMIT) | wc -l)),0)
	(cd c_src; git fetch; git checkout --force $(COMMIT))
endif
endif

# One rule to copy them all
$(PRIVEXECS): $(CUCKOO)/$$(@F)
	cp $(CUCKOO)/$(@F) $(PRIV)

# The args vary slightly so spell out the compilation rules
$(CUCKOO)/lean15-generic: $(HDRS) $(CUCKOO)/lean.hpp $(CUCKOO)/lean.cpp
	(cd $(CUCKOO); $(GPP) -o $(@F) -DATOMIC -DEDGEBITS=15 lean.cpp $(BLAKE_2B_SRC))

$(CUCKOO)/lean29-generic: $(HDRS) $(CUCKOO)/lean.hpp $(CUCKOO)/lean.cpp
	(cd $(CUCKOO); $(GPP) -o $(@F) -DATOMIC -DEDGEBITS=29 lean.cpp $(BLAKE_2B_SRC))

$(CUCKOO)/lean29-avx2: $(HDRS) $(CUCKOO)/lean.hpp $(CUCKOO)/lean.cpp
	(cd $(CUCKOO); $(GPP) -o $(@F) -DATOMIC -mavx2 -DNSIPHASH=8 -DEDGEBITS=29 lean.cpp $(BLAKE_2B_SRC))

$(CUCKOO)/mean15-generic: $(HDRS) $(CUCKOO)/mean.hpp $(CUCKOO)/mean.cpp
	(cd $(CUCKOO); $(GPP) -o $(@F) -DSAVEEDGES -DXBITS=0 -DNSIPHASH=1 -DEDGEBITS=15 mean.cpp $(BLAKE_2B_SRC))

$(CUCKOO)/mean29-generic: $(HDRS) $(CUCKOO)/mean.hpp $(CUCKOO)/mean.cpp
	(cd $(CUCKOO); $(GPP) -o $(@F) -DSAVEEDGES -DNSIPHASH=1 -DEDGEBITS=29 mean.cpp $(BLAKE_2B_SRC))

$(CUCKOO)/mean29-avx2: $(HDRS) $(CUCKOO)/mean.hpp $(CUCKOO)/mean.cpp
	(cd $(CUCKOO); $(GPP) -o $(@F) -DSAVEEDGES -mavx2 -DNSIPHASH=8 -DEDGEBITS=29 mean.cpp $(BLAKE_2B_SRC))

$(CUCKOO)/lcuda29:	$(CUCKOO)/../crypto/siphash.cuh $(CUCKOO)/lean.cu
	(cd $(CUCKOO); $(NVCC) -o $(@F) -DEDGEBITS=29 -arch sm_35 lean.cu $(BLAKE_2B_SRC))

$(CUCKOO)/cuda29:		$(CUCKOO)/../crypto/siphash.cuh $(CUCKOO)/mean.cu
	(cd $(CUCKOO); $(NVCC) -o $(@F) -DEDGEBITS=29 -arch sm_35 mean.cu $(BLAKE_2B_SRC))


# Create the private dir
$(PRIV):
	mkdir -p $@

# Clone without checking out, so that recipe interrupted after clone
# does not leave working directory distinct from the expected commit.
c_src/.git:
	git clone -n $(REPO) $(@D)
