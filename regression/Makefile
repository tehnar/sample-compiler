TESTS=test001 test002 test003 test004 test005 test006 test007 test008 test009 test010 test012 test013 test014 test015 test016 test017 test018 test019 test020 test021 test022 test023
INTERPRETER_TESTS=test011

.PHONY: check $(TESTS) $(INTERPRETER_TESTS) 

check: $(TESTS) $(INTERPRETER_TESTS) 

$(TESTS): %: %.expr
	RC_RUNTIME=../runtime ../rc.native -o  $< && cat $@.input | ./$@ > $@.log && diff $@.log orig/$@.log
	cat $@.input | ../rc.native -i $< > $@.log && diff $@.log orig/$@.log
	cat $@.input | ../rc.native -s $< > $@.log && diff $@.log orig/$@.log

$(INTERPRETER_TESTS): %: %.expr
	cat $@.input | ../rc.native -i $< > $@.log && diff $@.log orig/$@.log

clean:
	rm -f test*.log *.s *~ $(TESTS)
