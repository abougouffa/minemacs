EMACS_DIR=.

clean:
	rm -rf $(EMACS_DIR)/eln-cache $(EMACS_DIR)/local/cache $(EMACS_DIR)/local/straight/build-*

clean_all: clean
	rm -rf $(EMACS_DIR)/local/straight/

prune: clean
	rm -rf $(EMACS_DIR)/local

autoloads:
	rm $(EMACS_DIR)/core/me-autoloads.el
