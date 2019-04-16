PKG=verylog-hs
TARGETS=$(PKG):lib $(PKG):test:iodine-test
GHCICOMMAND=stack ghci $(TARGETS)

.PHONY: ghci ghcid

ghci:
	$(GHCICOMMAND)

ghcid:
	ghcid --command="$(GHCICOMMAND)"
