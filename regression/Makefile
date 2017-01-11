TESTS=test001 test002 test003 test004 test005 test006 test007 test008 test009 test010 test012 test013 test014 test015 test016 test017 test018 test019 test020 test021 test022 test023 test024 test025 test026 test027 test028 test029 test030 hand-test001 test011 hand-test002 hand-test003 hand-test004 hand-test005 hand-test006 hand-test007 hand-test008 hand-test009 hand-test010 hand-test011

INTERPRETER_TESTS=

.PHONY: check $(TESTS) $(INTERPRETER_TESTS) 

check: $(TESTS) $(INTERPRETER_TESTS) 


$(TESTS): %: %.expr
	cat $@.input | ../rc.native -i $< > $@.log && diff $@.log orig/$@.log
	cat $@.input | ../rc.native -s $< > $@.log && diff $@.log orig/$@.log
	RC_RUNTIME=../runtime ../rc.native -o  $< && cat $@.input | ./$@ > $@.log && diff $@.log orig/$@.log

$(INTERPRETER_TESTS): %: %.expr
	cat $@.input | ../rc.native -i $< > $@.log && diff $@.log orig/$@.log
	cat $@.input | ../rc.native -s $< > $@.log && diff $@.log orig/$@.log

clean:
	rm -f test*.log *.s *~ $(TESTS)
