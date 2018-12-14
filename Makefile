MODULE_NAME=verylog-hs-0.1.0.0
THIS_DIR=$(dir $(realpath $(firstword $(MAKEFILE_LIST))))

.PHONY: all

all:
	@stack build
	@echo "\nLOG:\n"
	@cat $(THIS_DIR)/.stack-work/logs/$(MODULE_NAME).log-color

