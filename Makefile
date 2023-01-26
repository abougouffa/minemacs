EMACS_DIR=.

all:
	echo "Possible options: clean, clean_all, prune, autoloads"

clean:
	rm -rf $(EMACS_DIR)/eln-cache $(EMACS_DIR)/local/eln-cache $(EMACS_DIR)/local/cache $(EMACS_DIR)/local/straight/build-*

clean_all: clean
	rm -rf $(EMACS_DIR)/local/straight/

prune: clean
	rm -rf $(EMACS_DIR)/local

autoloads:
	rm $(EMACS_DIR)/core/me-autoloads.el
