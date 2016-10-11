TESTS=$(basename $(wildcard generated*.expr))

.PHONY: check $(TESTS)

check: $(TESTS)

$(TESTS): %: %.expr
	RC_RUNTIME=../../runtime ../../src/rc.byte $< && cat $@.input | ./$@ > $@.log && diff $@.log orig/$@.log
	cat $@.input | RC_RUNTIME=../../runtime ../../src/rc.byte -i $< > $@.log && diff $@.log orig/$@.log
	cat $@.input | RC_RUNTIME=../../runtime ../../src/rc.byte -s $< > $@.log && diff $@.log orig/$@.log

clean:
	rm -f *.log *.s *~ $(TESTS)
