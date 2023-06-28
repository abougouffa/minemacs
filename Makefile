EMACS_DIR=.
EMACS=emacs
EMACS_BATCH=emacs --batch --script init.el

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
	$(EMACS_BATCH) --eval='(straight-pull-all)'

rebuild:
	$(EMACS_BATCH) --eval='(straight-rebuild-all)'

check:
	$(EMACS_BATCH) --eval='(straight-check-all)'

update:
	$(EMACS_BATCH) --eval='(minemacs-update)'

cloc:
	cloc --match-f='\.el$$' init.el early-init.el elisp/ modules/ core/
