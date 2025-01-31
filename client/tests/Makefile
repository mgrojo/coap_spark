results.md : ../bin/coap_client coap_client_tests.md
	bbt --verbose coap_client_tests.md | tee $@

clean:
	rm -f results.md
.PHONY: clean
