.PHONY: all
all: fate_eval

.PHONY: fate_eval
fate_eval:
	escript priv/aefa_gen_dispatch src/aefa_fate_eval.erl

.PHONY: clean
clean:
	-rm src/aefa_fate_eval.erl
