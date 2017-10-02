
## Requires ocaml >= 4.02, < 4.06
## and reason-3.0.0 (opam install reason).

default : voting_test

%.ml : %.re
	refmt -p ml $< > $@


voting_test : rte.ml voting.ml voting_test.ml
	ocamlopt -o $@ $^

clean :
	rm -f *.cmi *.cmx *.ml *.o voting_test
