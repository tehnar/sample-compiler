TESTS=$(basename $(wildcard generated*.expr))

.PHONY: check runtime $(TESTS)

runtime: 
	cp ../../runtime/runtime.o ./

check: runtime $(TESTS) 

$(TESTS): %: %.expr
	../../src/rc.byte -o $< && cat $@.input | ./$@ > $@.log && diff $@.log orig/$@.log
	cat $@.input | RC_RUNTIME=../../runtime ../../src/rc.byte -i $< > $@.log && diff $@.log orig/$@.log
	cat $@.input | RC_RUNTIME=../../runtime ../../src/rc.byte -s $< > $@.log && diff $@.log orig/$@.log
	rm $@.log $@ $@.s

clean:
	rm -f *.log *.s *~ $(TESTS)
