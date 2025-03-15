ifeq ($(OS),Windows)
    RM = del /Q
    PROGRAM = ..\bin\coap_client.exe
else
    RM = rm -f
    PROGRAM = ../bin/coap_client
endif

results.md : $(PROGRAM) coap_client_tests.md
	bbt --verbose coap_client_tests.md | tee $@

valgrind : $(PROGRAM) coap_client_tests.md
	valgrind --tool=memcheck --log-file=valgrind-%p.txt --trace-children=yes bbt --verbose coap_client_tests.md
.PHONY: valgrind

clean:
	$(RM) results.md
.PHONY: clean
