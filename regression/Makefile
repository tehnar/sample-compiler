TESTS=test001 test002 test003 test004 test005 

.PHONY: check $(TESTS)

check: $(TESTS)

$(TESTS): %: %.expr
	cp ../runtime/runtime.o ./
	../src/rc.byte -o $< && cat $@.input | ./$@ > $@.log && diff $@.log orig/$@.log
	cat $@.input | ../src/rc.byte    $< > $@.log && diff $@.log orig/$@.log
	cat $@.input | ../src/rc.byte -s $< > $@.log && diff $@.log orig/$@.log

clean:
	rm *.log *.s
