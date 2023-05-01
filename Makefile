EMACS_DIR=.
EMACS=emacs

all:
	@echo "Cleaning options are: clean, clean_all, prune, loaddefs."
	@echo "Straight options are: pull, rebuild, check."

clean:
	rm -rf $(EMACS_DIR)/eln-cache $(EMACS_DIR)/local/eln-cache $(EMACS_DIR)/local/cache $(EMACS_DIR)/local/straight/build-*

clean_pcache:
	rm -rf $(EMACS_DIR)/local/cache/pcache

clean_all: clean
	cp $(EMACS_DIR)/local/straight/versions/default.el /tmp/straight-versions-default.el
	rm -rf $(EMACS_DIR)/local/straight/
	mkdir -p $(EMACS_DIR)/local/straight/versions/
	cp /tmp/straight-versions-default.el $(EMACS_DIR)/local/straight/versions/default.el

prune: clean
	cp $(EMACS_DIR)/local/straight/versions/default.el /tmp/straight-versions-default.el
	rm -rf $(EMACS_DIR)/local
	mkdir -p $(EMACS_DIR)/local/straight/versions/
	cp /tmp/straight-versions-default.el $(EMACS_DIR)/local/straight/versions/default.el

loaddefs:
	rm $(EMACS_DIR)/core/me-loaddefs.el

pull:
	$(EMACS) --eval='(straight-pull-all)'

rebuild:
	$(EMACS) --eval='(straight-rebuild-all)'

check:
	$(EMACS) --eval='(straight-check-all)'
