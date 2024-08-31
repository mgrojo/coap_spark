RFLX_HOME := tools/RecordFlux/.venv/bin

convert_iana: spec/iana_registry/core-parameters.xml
	$(RFLX_HOME)/rflx convert iana -d spec $<
.PHONY: convert_iana