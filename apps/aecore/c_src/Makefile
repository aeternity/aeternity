.PHONY: deps deps_build clean distclean help test

CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)

PROJECT := aec_pow_cuckoo

LIBDIR := cuckoo

ERTS_INCLUDE_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~s/erts-~s/include/\", [code:root_dir(), erlang:system_info(version)]).")

C_SRC_DIR = $(CURDIR)
PRIV_DIR = $(BASEDIR)/priv/
LIB20_SO ?= $(CURDIR)/../priv/$(PROJECT)20_nif.so
LIB28_SO ?= $(CURDIR)/../priv/$(PROJECT)28_nif.so
LIB32_SO ?= $(CURDIR)/../priv/$(PROJECT)32_nif.so

# System type and C compiler/flags.

UNAME_SYS := $(shell uname -s)
ifeq ($(UNAME_SYS), Darwin)
	CC ?= cc
	CFLAGS ?= -fPIC -O3 -std=c99 -arch x86_64 -Wall -Wmissing-prototypes
	CXXFLAGS ?= -O3 -arch x86_64 -Wall
	LDFLAGS ?= -arch x86_64 -flat_namespace -undefined suppress
	SED_FLAGS ?= -i ''
else ifeq ($(UNAME_SYS), FreeBSD)
	CC = clang
	CXX = clang++
	CFLAGS ?= -fPIC -O3 -std=c99 -Wall -Wmissing-prototypes
	CFLAGS += -D_GCLIBCXX_USE_C99
	CXXFLAGS ?= -O3 -Wall
	CXXFLAGS += -std=c++11 -stdlib=libc++ -D_GCLIBCXX_USE_C99
	SED_FLAGS ?= -i
else ifeq ($(UNAME_SYS), OpenBSD)
	CC = egcc
	CXX = eg++
	CFLAGS ?= -fPIC -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
	CFLAGS += -D_GCLIBCXX_USE_C99
	CXXFLAGS ?= -O3 -Wall
	CXXFLAGS += -std=c++11 -pthread -finline-functions -D_GCLIBCXX_USE_C99
	SED_FLAGS ?= -i
else ifeq ($(UNAME_SYS), DragonFly)
	CC = c++48
	CXX = c++48
	CFLAGS ?= -fPIC -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
	CFLAGS += -D_GCLIBCXX_USE_C99
	CXXFLAGS ?= -O3 -finline-functions -Wall
	CXXFLAGS += -D_GCLIBCXX_USE_C99
	SED_FLAGS ?= -i
else ifeq ($(UNAME_SYS), NetBSD)
	CC = c++48
	CXX = c++48
	CFLAGS ?= -fPIC -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
	CFLAGS += -D_GCLIBCXX_USE_C99
	CXXFLAGS ?= -O3 -finline-functions -Wall
	CXXFLAGS += -D_GCLIBCXX_USE_C99
	SED_FLAGS ?= -i
else ifeq ($(UNAME_SYS), SunOS)
	ifeq ($(shell uname -v | sed 's/_.*$$//'), joyent)
		CC = gcc
		CXX = g++
	else
		CC = c++48
		CXX = c++48
	endif
	CFLAGS ?= -fPIC -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
	CFLAGS += -D_GCLIBCXX_USE_C99
	CXXFLAGS ?= -O3 -finline-functions -Wall
	CXXFLAGS += -D_GCLIBCXX_USE_C99
	SED_FLAGS ?= -i
else ifeq ($(UNAME_SYS), Linux)
	CC ?= gcc
	CFLAGS ?= -fPIC -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS ?= -O3 -finline-functions -Wall
	SED_FLAGS ?= -i
endif

ifeq ($(UNAME_SYS), OpenBSD)
	LDLIBS += -lestdc++
else
	LDLIBS += -lstdc++
endif

LDFLAGS += -shared

CXXFLAGS += -fPIC -I $(ERTS_INCLUDE_DIR) -I. -I./cuckoo/src -std=c++11 -Wno-format

# Verbosity.

c_verbose_0 = @echo " C     " $(?F);
c_verbose = $(c_verbose_$(V))

cpp_verbose_0 = @echo " CPP   " $(?F);
cpp_verbose = $(cpp_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

OBJECTS20 = aec_pow_cuckoo.20o aec_pow_cuckoo_nif.20o
OBJECTS28 = aec_pow_cuckoo.28o aec_pow_cuckoo_nif.28o
OBJECTS32 = aec_pow_cuckoo.32o aec_pow_cuckoo_nif.32o
CUCKOO_SRCS = $(LIBDIR)/src/blake2b-ref.c
CUCKOO_OBJECTS = $(LIBDIR)/src/blake2b-ref.o

COMPILE_C = $(c_verbose) $(CC) $(CFLAGS) $(CPPFLAGS) -c
COMPILE_CPP = $(cpp_verbose) $(CXX) $(CXXFLAGS) $(CPPFLAGS) -c

help: ## This documentation
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

all: deps $(PRIV_DIR) $(LIB20_SO) $(LIB28_SO) # $(LIB32_SO) ## Build all

$(LIB20_SO): $(OBJECTS20) $(CUCKOO_OBJECTS)
	$(link_verbose) $(CXX) -o $@ $(LDLIBS)  $(LDFLAGS) $^

$(LIB28_SO): $(OBJECTS28) $(CUCKOO_OBJECTS)
	$(link_verbose) $(CXX) -o $@ $(LDLIBS)  $(LDFLAGS) $^

# $(LIB32_SO): $(OBJECTS32) $(CUCKOO_OBJECTS)
# 	$(link_verbose) $(CXX) -o $@ $(LDLIBS)  $(LDFLAGS) $^

%.o: %.c
	$(COMPILE_C) $(OUTPUT_OPTION) $<

%.20o: %.cpp
	$(COMPILE_CPP) -DEDGEBITS=19 $(OUTPUT_OPTION) $<

%.28o: %.cpp
	$(COMPILE_CPP) -DEDGEBITS=27 $(OUTPUT_OPTION) $<

# %.32o: %.cpp
# 	$(COMPILE_CPP) -DEDGEBITS=31 $(OUTPUT_OPTION) $<

deps: $(LIBDIR)

$(LIBDIR):
	git clone https://github.com/tromp/cuckoo.git
	( cd $(LIBDIR) && git checkout bded215; )
	sed $(SED_FLAGS) 's/printf/aec_printf/g' $(LIBDIR)/src/cuckoo_miner.hpp
	sed $(SED_FLAGS) 's/.*TWICE_PER_ATOM = .*/int aec_printf(const char *format, ...);&/g' $(LIBDIR)/src/cuckoo_miner.hpp

$(PRIV_DIR):
	mkdir -p $(PRIV_DIR)

$(CUCKOO_SRCS): $(LIBDIR)

$(CUCKOO_OBJECTS): $(CUCKOO_SRCS)

clean: ## Clean all files
	@rm -f $(OBJECTS) $(LIB_SO)

distclean: clean
	@rm -fR $(LIBDIR)
