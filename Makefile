RFLX_HOME := tools/RecordFlux/
RFLX := $(RFLX_HOME)/.venv/bin/rflx
SPECS := specs/coap_client.rflx # $(wildcard specs/*.rflx)
GNATPROVE := gnatprove

GENERATED := $(patsubst specs/%,generated/%,$(SPECS:.rflx=.ads))

all: generate
.PHONY: all

# Beware that for CoAP, the only useful type generated by
# its IANA registry is the CoAP Option Code, and we are
# copying it to the manually written coap.rflx, instead of
# including the generated one.
convert_iana: specs/iana_registry/core-parameters.xml
	$(RFLX) convert iana -d specs $<
	specs/iana_registry/convert_content_formats.awk $< \
		> src/coap_spark-content_formats.ads
.PHONY: convert_iana

generate: $(GENERATED)
.PHONY: generate

generated/%.ads: specs/%.rflx
	@echo "Generating $@ from $<" 
	@mkdir -p $(dir $@)
	@$(RFLX) generate -d $(dir $@) $<
	@$(RFLX) graph -d $(dir $@) $<

test:
	$(MAKE) -C tests test
.PHONY: test

clean:
	rm -r $(dir $(GENERATED))
.PHONY: clean

prove: $(GENERATED)
	$(GNATPROVE) -Pcoap_spark.gpr
.PHONY: prove
