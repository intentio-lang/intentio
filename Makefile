ALL_HS_FILES := $(shell sh -c ' rg --files | rg .hs$ ')

.PHONY: fmt lint

lint:
	hlint -gj --cross

fmt:
	brittany --write-mode inplace --output-on-errors $(ALL_HS_FILES)
