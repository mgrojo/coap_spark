results.md : ../bin/coap_client coap_client_tests.md
	strace -f bbt --verbose coap_client_tests.md | tee $@

valgrind : ../bin/coap_client coap_client_tests.md
	valgrind --tool=memcheck --log-file=valgrind-%p.txt --trace-children=yes bbt --verbose coap_client_tests.md
.PHONY: valgrind

clean:
	rm -f results.md
.PHONY: clean
