EXECUTABLES = \
	mean29-generic \
	mean15-generic \
	mean29-sse2 \
	mean29-avx2 \
	lean29 \
	lean15
PRIVEXECS = $(addprefix $(PRIV)/, $(EXECUTABLES))

PRIV=priv/bin
CUCKOO=c_src/src/cuckoo

HDRS=$(CUCKOO)/cuckoo.h $(CUCKOO)/../crypto/siphash.h

# Flags from upstream makefile
OPT ?= -O3

GPP_ARCH_FLAGS ?= -m64 -x c++ -march=native

# -Wno-deprecated-declarations shuts up Apple OSX clang
FLAGS ?= -Wall -Wno-format -Wno-deprecated-declarations -D_POSIX_C_SOURCE=200112L $(OPT) -DPREFETCH -I. $(CPPFLAGS) -pthread
GPP ?= g++ $(GPP_ARCH_FLAGS) -std=c++11 $(FLAGS)
BLAKE_2B_SRC ?= ../crypto/blake2b-ref.c

# end Flags from upstream

REPO = https://github.com/aeternity/cuckoo.git
COMMIT = 51351332d23a11bc30e2457bd98f1c2ba358eb1e
#COMMIT = 4e1e308fa597645ae086d714ee1f66b9a7b08e6a

.PHONY: all
all: $(EXECUTABLES)
	@: # Silence the `Nothing to be done for 'all'.` message when running `make all`.

.PHONY: clean
clean:
	(cd $(PRIV); rm -f $(EXECUTABLES))
	(cd $(CUCKOO); rm -f $(EXECUTABLES))

.PHONY: distclean
distclean:
	rm -rf c_src priv

.SECONDEXPANSION:
.PHONY: $(EXECUTABLES)
$(EXECUTABLES): | c_src/.git $(PRIV)
$(EXECUTABLES): git-version $(CUCKOO)/$$@ $(PRIV)/$$@

# So we need check out the right commit or cleanup if
# pointing to the wrong/old/dirty thing.
.PHONY: git-version
git-version:
ifneq ($(strip $(shell cd c_src; git rev-parse HEAD)),$(COMMIT))
	(cd c_src; git fetch; git checkout --force $(COMMIT))
else
ifneq ($(strip $(shell cd c_src; git diff-index $(COMMIT) | wc -l)),0)
	(cd c_src; git fetch; git checkout --force $(COMMIT))
endif
endif

# One rule to copy them all
$(PRIVEXECS): $(CUCKOO)/$$(@F)
	cp $(CUCKOO)/$(@F) $(PRIV)

# The args vary slightly so spell out the compilation rules
$(CUCKOO)/lean15: $(HDRS) $(CUCKOO)/lean.hpp $(CUCKOO)/lean.cpp
	(cd $(CUCKOO); $(GPP) -o $(@F) -DATOMIC -DEDGEBITS=15 lean.cpp $(BLAKE_2B_SRC))

$(CUCKOO)/lean29: $(HDRS) $(CUCKOO)/lean.hpp $(CUCKOO)/lean.cpp
	(cd $(CUCKOO); $(GPP) -o $(@F) -DATOMIC -DEDGEBITS=29 lean.cpp $(BLAKE_2B_SRC))

$(CUCKOO)/mean15-generic: $(HDRS) $(CUCKOO)/lean.hpp $(CUCKOO)/lean.cpp
	(cd $(CUCKOO); $(GPP) -o $(@F) -DSAVEEDGES -DXBITS=0 -DNSIPHASH=1 -DEDGEBITS=15 mean.cpp $(BLAKE_2B_SRC))

$(CUCKOO)/mean29-generic: $(HDRS) $(CUCKOO)/mean.hpp $(CUCKOO)/mean.cpp
	(cd $(CUCKOO); $(GPP) -o $(@F) -DSAVEEDGES -DNSIPHASH=1 -DEDGEBITS=29 mean.cpp $(BLAKE_2B_SRC))

$(CUCKOO)/mean29-sse2: $(HDRS) $(CUCKOO)/mean.hpp $(CUCKOO)/mean.cpp
	(cd $(CUCKOO); $(GPP) -o $(@F) -DSAVEEDGES -mno-avx2 -DNSIPHASH=4 -DEDGEBITS=29 mean.cpp $(BLAKE_2B_SRC))

$(CUCKOO)/mean29-avx2: $(HDRS) $(CUCKOO)/mean.hpp $(CUCKOO)/mean.cpp
	(cd $(CUCKOO); $(GPP) -o $(@F) -DSAVEEDGES -mavx2 -DNSIPHASH=8 -DEDGEBITS=29 mean.cpp $(BLAKE_2B_SRC))

# Create the private dir
$(PRIV):
	mkdir -p $@

# Clone without checking out, so that recipe interrupted after clone
# does not leave working directory distinct from the expected commit.
c_src/.git:
	git clone -n $(REPO) $(@D)

