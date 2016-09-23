TESTS=test001 test002 test003

.PHONY: check $(TESTS)
check: $(TESTS)

$(TESTS): %: %.expr %.input orig/%.log
	../src/rc.byte $@.expr && cat $@.input | ./$@ > $@.log && diff $@.log orig/$@.log

clean:
	rm *.log *.s *~
