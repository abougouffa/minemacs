EMACS_DIR=.
EMACS=emacs
EMACS_BATCH=emacs --batch --script init.el
CLOC=cloc

all:
	@echo "Cleaning rules are: clean, clean-extras, clean-all, clean-cache, prune, clean-loaddefs."
	@echo "MinEmacs packages rules: bump, upgrade."
	@echo "MinEmacs CI rules: ci, ci-daemon."
	@echo "Extra rules: cloc."
	@echo "Documentation options: gen-external-tools, gen-descriptions, documentation."

clean-extras:
	rm -rf $(EMACS_DIR)/local/parinfer-rust || true
	rm -rf $(EMACS_DIR)/local/tree-sitter || true
	rm -rf $(EMACS_DIR)/local/lsp || true
	rm -rf $(EMACS_DIR)/local/ltex-ls-plus* || true
	rm -rf $(EMACS_DIR)/local/copilot || true
	rm -rf $(EMACS_DIR)/local/whisper || true
	rm -rf $(EMACS_DIR)/local/devdocs || true
	rm -rf $(EMACS_DIR)/local/tldr || true
	rm -rf $(EMACS_DIR)/local/powershell || true
	rm -rf $(EMACS_DIR)/local/clean_extras || true

clean-cache: clean-extras
	rm -rf $(EMACS_DIR)/local/ellama-sessions || true
	rm -rf $(EMACS_DIR)/local/tramp-auto-save || true
	rm -rf $(EMACS_DIR)/local/xkcd || true
	rm -rf $(EMACS_DIR)/local/undo-fu-session || true
	rm -rf $(EMACS_DIR)/local/real-backup || true
	rm -rf $(EMACS_DIR)/local/pscratch || true
	rm -rf $(EMACS_DIR)/local/logview-cache.extmap || true
	rm -rf $(EMACS_DIR)/local/desktop-session || true
	rm -rf $(EMACS_DIR)/local/auto-save || true
	rm -rf $(EMACS_DIR)/local/auto-insert || true
	rm -rf $(EMACS_DIR)/local/backup || true
	rm -rf $(EMACS_DIR)/local/beardbolt-sandbox || true
	rm $(EMACS_DIR)/local/dape-breakpoints || true
	rm $(EMACS_DIR)/local/ditaa*.jar || true
	rm $(EMACS_DIR)/local/forge-database*.sqlite || true

clean: clean-extras
	rm -rf $(EMACS_DIR)/eln-cache $(EMACS_DIR)/local/eln-cache $(EMACS_DIR)/local/cache $(EMACS_DIR)/local/straight/build-*

clean-all: clean clean-cache
	cp $(EMACS_DIR)/local/straight/versions/default.el /tmp/straight-versions-default.el
	rm -rf $(EMACS_DIR)/local/straight/
	mkdir -p $(EMACS_DIR)/local/straight/versions/
	cp /tmp/straight-versions-default.el $(EMACS_DIR)/local/straight/versions/default.el

prune: clean
	cp $(EMACS_DIR)/local/straight/versions/default.el /tmp/straight-versions-default.el
	rm -rf $(EMACS_DIR)/local
	mkdir -p $(EMACS_DIR)/local/straight/versions/
	cp /tmp/straight-versions-default.el $(EMACS_DIR)/local/straight/versions/default.el

clean-loaddefs:
	rm $(EMACS_DIR)/core/me-loaddefs.el

bump:
	$(EMACS_BATCH) --eval='(minemacs-bump-packages)'

upgrade:
	$(EMACS_BATCH) --eval='(minemacs-upgrade nil)'

build:
	$(EMACS_BATCH) --eval='(minemacs-run-build-functions t)'

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
	cat init.el early-init.el core/me-vars.el core/me-lib.el core/me-lib-x.el core/me-builtin.el core/extras/me-*.el modules/me-*.el modules/extras/me-*.el >combined.el

documentation: gen-descriptions gen-external-tools combined.el make-readme-markdown.el
	emacs --script make-readme-markdown.el <combined.el >docs/functions-and-commands.md 2>/dev/null

.INTERMEDIATE: make-readme-markdown.el combined.el
