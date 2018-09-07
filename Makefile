.PHONY: fmt

fmt:
	brittany --write-mode inplace --output-on-errors \
		$(shell sh -c ' rg --files | rg .hs$ ')
