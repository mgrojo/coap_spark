ifeq ($(OS),Windows)
    RM = del /Q
    PROGRAM = ..\bin\coap_client.exe
else
    RM = rm -f
    PROGRAM = ../bin/coap_client
endif

RESULTS = coap_results.md coaps_results.md

all: $(RESULTS)
	@echo "All tests run."
.PHONY: all

coap_results.md : coap_client_tests.md $(PROGRAM)
	bbt --verbose $< | tee $@

coaps_results.md: coaps_client_tests.md $(PROGRAM)
	coap-server-openssl -k COAP_SPARK_KEY_5684 -u coap_spark -d 100 &
	bbt --verbose $< | tee $@
	pkill -f coap-server-openssl

valgrind : $(PROGRAM) coap_client_tests.md
	valgrind --tool=memcheck --log-file=valgrind-%p.txt --trace-children=yes bbt --verbose coap_client_tests.md
.PHONY: valgrind

clean:
	$(RM) $(RESULTS) valgrind-*.txt
	@echo "Cleaned up."
.PHONY: clean
