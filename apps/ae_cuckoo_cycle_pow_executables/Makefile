EXECUTABLES = \
	priv/bin/mean16 \
	priv/bin/lean16 \
	priv/bin/verify16 \
	priv/bin/mean28 \
	priv/bin/lean28 \
	priv/bin/verify28 \
	priv/bin/mean30 \
	priv/bin/lean30 \
	priv/bin/verify30

LIBBLAKE = priv/lib/libblake2b.so

SRC_EXECUTABLES = $(subst priv/bin, c_src/src, $(EXECUTABLES))

SRC_LIBBLAKE = $(subst priv/lib, c_src/src, $(LIBBLAKE))

REPO = https://github.com/aeternity/cuckoo.git
COMMIT = fddbf60

.PHONY: all clean distclean
all: 	$(LIBBLAKE) $(EXECUTABLES)

clean:
	rm -f $(SRC_EXECUTABLES) $(EXECUTABLES) $(SRC_LIBBLAKE) $(LIBBLAKE)

distclean:
	rm -rf c_src priv

$(LIBBLAKE): $(SRC_LIBBLAKE) priv/lib
	cp -p $< $@

$(EXECUTABLES): priv/bin/%: c_src/src/% priv/bin
	cp -p $< $@

priv/lib priv/bin:
	mkdir -p $@

$(SRC_LIBBLAKE): c_src/.git priv/lib priv/bin
	( cd $(<D) && git checkout -q $(COMMIT); )
	( cd $(@D) && $(MAKE) -s $(@F); )

c_src/src/%: c_src/.git priv/lib priv/bin
	( cd $(@D) && $(MAKE) $*; )

c_src/.git:
	git clone $(REPO) $(@D)
