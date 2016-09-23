TESTS=test001 test002 test003 test004 test005 

.PHONY: check $(TESTS)

check: $(TESTS)

$(TESTS): 
	../src/rc.byte $@.expr && cat $@.input | ./$@ > $@.log && diff $@.log orig/$@.log

clean:
	rm *.log *.s
