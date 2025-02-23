RFLX_HOME := ../tools/RecordFlux/
PATH := $(RFLX_HOME)/.venv/bin:$(PATH)

test:
	$(RFLX_HOME)/tools/extract_packets.py --payload inet.UDP samples/coap-cbor.pcap samples/
	rflx validate -v samples/ ../specs/coap.rflx CoAP::CoAP_Message
.PHONY: test