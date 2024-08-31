convert_iana: spec/iana_registry/core-parameters.xml
	tools/RecordFlux/.venv/bin/rflx convert iana $<
.PHONY: convert_iana