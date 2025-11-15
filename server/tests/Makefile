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
SECURE_ARGS = -k COAP_SPARK_KEY_5684 -u coap_spark

all: $(RESULTS)
	@echo "All tests run."
.PHONY: all

# Run the server in the background, run the tests, check the results, dump the
# coverage report, let the report to be dumped while sleeping and finally kill
# the server.
coap_results.md : coap_server_tests.md $(PROGRAM)
	$(PROGRAM) -v 4 & \
    SERVER_PID=$$!; \
    trap 'sleep 2; kill $$SERVER_PID >/dev/null 2>&1 || true' EXIT; \
    bbt --keep-going --verbose $< > $@; \
    grep -F 'Summary : **Success**' $@; \
    coap_client -m fetch coap://localhost/

# Secure tests
coaps_results.md: coaps_server_tests.md $(PROGRAM)
	$(PROGRAM) -v 4 $(SECURE_ARGS) & \
    SERVER_PID=$$!; \
    trap 'sleep 2; kill $$SERVER_PID >/dev/null 2>&1 || true' EXIT; \
    bbt --keep-going --verbose $< > $@; \
    grep -F 'Summary : **Success**' $@; \
    coap_client -m fetch $(SECURE_ARGS) coaps://localhost/

valgrind : $(PROGRAM) coap_server_tests.md
	valgrind --tool=memcheck --log-file=valgrind-%p.txt --trace-children=yes bbt --verbose coap_server_tests.md
.PHONY: valgrind

clean:
	$(RM) $(RESULTS) valgrind-*.txt
	@echo "Cleaned up."
.PHONY: clean
