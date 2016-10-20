TESTS=$(basename $(wildcard generated*.expr))

.PHONY: check runtime $(TESTS) $(INTERPRETER_TESTS) 

check: runtime $(TESTS)

runtime: 
	cp ../../runtime/runtime.o ./

$(TESTS): %: %.expr
	../../rc.native -o $< && cat $@.input | ./$@ > $@.log && diff $@.log orig/$@.log
	cat $@.input | RC_RUNTIME=../../runtime ../../rc.native -i $< > $@.log && diff $@.log orig/$@.log
	cat $@.input | RC_RUNTIME=../../runtime ../../rc.native -s $< > $@.log && diff $@.log orig/$@.log
	rm $@.log $@ $@.s

clean:
	rm -f *.log *.s *~ $(TESTS)
