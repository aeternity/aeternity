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

REPO = https://github.com/aeternity/cuckoo.git
COMMIT = fddbf60

.PHONY: all
all: \
	priv/lib/libblake2b.so \
	$(EXECUTABLES)

.PHONY: clean
clean:
	rm -rf c_src priv

priv/lib/libblake2b.so: c_src/src/libblake2b.so priv/lib
	cp -p $< $@

$(EXECUTABLES): priv/bin/%: c_src/src/% priv/bin
	cp -p $< $@

priv/lib priv/bin:
	mkdir -p $@

c_src/src/libblake2b.so: c_src/.git
	( cd $(<D) && git checkout -q $(COMMIT); )
	( cd $(@D) && $(MAKE) -s libblake2b.so; )

c_src/src/%: c_src/.git
	( cd $(<D) && git checkout -q $(COMMIT); )
	( cd $(@D) && $(MAKE) -s libblake2b.so $*; )

c_src/.git:
	git clone $(REPO) $(@D)
