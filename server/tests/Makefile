ifeq ($(OS),Windows)
    RM = del /Q
    PROGRAM = ..\bin\coap_server.exe
	CLIENT_HOME = ..\..\client\bin
else
    RM = rm -f
    PROGRAM = ../bin/coap_server
	CLIENT_HOME = ../../client/bin
endif

RESULTS = coap_results.md coaps_results.md

all: $(RESULTS)
	@echo "All tests run."
.PHONY: all

coap_results.md : coap_server_tests.md $(PROGRAM)
	bbt --verbose $< > $@
	grep -F 'Summary : **Success**' $@

coaps_results.md: coaps_server_tests.md $(PROGRAM)
	$(PROGRAM) -k COAP_SPARK_KEY_5684 -u coap_spark &
	PATH=$(CLIENT_HOME):$$PATH bbt --keep-going --verbose $< > $@
	pkill -f coap_server
	grep -F 'Summary : **Success**' $@

valgrind : $(PROGRAM) coap_server_tests.md
	valgrind --tool=memcheck --log-file=valgrind-%p.txt --trace-children=yes bbt --verbose coap_server_tests.md
.PHONY: valgrind

clean:
	$(RM) $(RESULTS) valgrind-*.txt
	@echo "Cleaned up."
.PHONY: clean
