SHELL := bash
.SHELLFLAGS := -eu -o pipefail -c

ifeq ($(OS),Windows)
    EXT := .exe
else
    EXT :=
endif

RM = rm -f
PROGRAM = ../bin/coap_server$(EXT)
CLIENT_HOME = ../../client/bin
PATH := $(CLIENT_HOME):$(PATH)
export PATH

RESULTS = coap_results.md coaps_results.md

all: $(RESULTS)
	@echo "All tests run."
.PHONY: all

coap_results.md : coap_server_tests.md $(PROGRAM)
	bbt --verbose $< > $@
	grep -F 'Summary : **Success**' $@

coaps_results.md: coaps_server_tests.md $(PROGRAM)
	$(PROGRAM) -v 4 -k COAP_SPARK_KEY_5684 -u coap_spark & \
    SERVER_PID=$$!; \
    trap 'kill $$SERVER_PID >/dev/null 2>&1 || true' EXIT; \
    bbt --keep-going --verbose $< > $@; \
    grep -F 'Summary : **Success**' $@

valgrind : $(PROGRAM) coap_server_tests.md
	valgrind --tool=memcheck --log-file=valgrind-%p.txt --trace-children=yes bbt --verbose coap_server_tests.md
.PHONY: valgrind

clean:
	$(RM) $(RESULTS) valgrind-*.txt
	@echo "Cleaned up."
.PHONY: clean
