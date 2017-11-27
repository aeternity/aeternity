
## Requires ocaml >= 4.02, < 4.06
## and reason-3.0.0 (opam install reason).

default : voting_test

%.ml : %.re
	refmt -p ml $< > $@

%.o : %.ml
	ocamlopt -c $<

voting_test : rte.o voting.o voting_test.o
	ocamlopt -o $@ $^
