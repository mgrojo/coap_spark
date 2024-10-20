RFLX_HOME := ../tools/RecordFlux/
RFLX := $(RFLX_HOME)/.venv/bin/rflx

test:
	$(RFLX_HOME)/tools/extract_packets.py --payload inet.UDP samples/coap-cbor.pcap samples/
	rflx validate -v samples/ ../specs/coap.rflx CoAP::CoAP_Message
.PHONY: test