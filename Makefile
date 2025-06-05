EMACS_DIR=.
EMACS=emacs
EMACS_BATCH=emacs --batch --script init.el
CLOC=cloc

all:
	@echo "Cleaning options are: clean, clean_extras, clean_all, prune, loaddefs."
	@echo "Straight options are: pull, rebuild, check."
	@echo "Extra options are: bump, cloc, locked, ci."
	@echo "Documentation options: gen-external-tools, gen-descriptions, documentation."

clean_extras:
	rm -rf $(EMACS_DIR)/local/parinfer-rust || true
	rm -rf $(EMACS_DIR)/local/tree-sitter || true
	rm -rf $(EMACS_DIR)/local/lsp || true
	rm -rf $(EMACS_DIR)/local/extra-packages || true
	rm -rf $(EMACS_DIR)/local/clean_extras || true

clean: clean_extras
	rm -rf $(EMACS_DIR)/eln-cache $(EMACS_DIR)/local/eln-cache $(EMACS_DIR)/local/cache $(EMACS_DIR)/local/straight/build-*

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

bump:
	$(EMACS_BATCH) --eval='(minemacs-bump-packages)'

upgrade:
	$(EMACS_BATCH) --eval='(minemacs-upgrade nil)'

cloc:
	$(CLOC) --match-f='\.el$$' init.el early-init.el elisp/ modules/ core/ skel/

ci:
	HOME=$(PWD)/.. $(EMACS) -nw --batch --script .github/workflows/scripts/ci-init.el 2>&1

ci-daemon:
	HOME=$(PWD)/.. timeout 180 $(EMACS) --daemon 2>&1

gen-descriptions:
	$(EMACS_BATCH) --eval='(progn (minemacs-extract-packages-descriptions) (with-current-buffer (get-buffer "*minemacs-modules-pkg-desc*") (write-file "docs/packages.md")))'

gen-external-tools:
	$(EMACS_BATCH) --eval='(progn (+list-external-dependencies) (with-current-buffer (get-buffer "*external-dependencies*") (write-file "docs/external-tools.md")))'

make-readme-markdown.el:
	wget -q -O $@ https://raw.github.com/mgalgs/make-readme-markdown/master/make-readme-markdown.el

combined.el:
	cat init.el early-init.el core/me-vars.el core/me-lib.el core/me-lib-extra.el core/me-builtin.el modules/me-*.el modules/extras/me-*.el >combined.el

documentation: gen-descriptions gen-external-tools combined.el make-readme-markdown.el
	emacs --script make-readme-markdown.el <combined.el >docs/functions-and-commands.md 2>/dev/null

.INTERMEDIATE: make-readme-markdown.el combined.el
