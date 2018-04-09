MODULE_NAME=verylog-hs-0.1.0.0

.PHONY: all

all:
	@stack install --fast
	@echo "\nLOG:\n"
	@cat .stack-work/logs/$(MODULE_NAME).log-color

