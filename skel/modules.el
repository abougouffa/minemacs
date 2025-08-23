;;; modules.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025 Abdelhak Bougouffa

;; This file can be used to override `minemacs-modules'

;;; List of enabled modules
(setq minemacs-modules
      '(;; me-ai          ; AI assistant using Ollama (ellama, llm, ...)
        ;; me-calendar    ; Calendar (org-timeblock, org-caldav, ...)
        me-checkers       ; Static checkers (flymake-collection, flymenu, ...)
        me-completion     ; Completion (vertico, marginalia, corfu, cape, consult, embark, ...)
        me-debug          ; Debugging tools (dape, rmsbolt, beardbolt, ...)
        me-docs           ; Documents (pdf-tools, pdfgrep, rfc-mode, devdocs, dash-docs, ...)
        me-editor         ; Editing (iedit, multiple-cursors, expreg, vundo, ws-butler, dtrt-indent, ...)
        me-emacs-lisp     ; Emacs lisp development (parinfer-rust, eros, ...)
        ;; me-email       ; Email (mu4e, mu4e-alert, org-msg, ...)
        ;; me-embedded    ; Embedded systems (embed, bitbake, ...)
        me-extra          ; Extra features (crux, pscratch, ...)
        me-files          ; Files and directories (dired-hacks, guard-lf, sudo-edit, ztree, trashed, ...)
        me-fun            ; Games and funny packages (xkcd, speed-type, wordel, ...)
        ;; me-lifestyle   ; *Very* opinionated lifestyle packages (awqat, ...)
        me-media          ; Multimedia (empv, ready-player, ...)
        me-natural-langs  ; Natural language stuff (jinx, flyspell-correct, lexic, reverso, ...)
        me-nav            ; Search and navigation (ace-window, avy, rg, fzf, dogears, goto-chg, p-search, phi-search, ...)
        me-notes          ; Notes & Zettelkasten (denote, consult-denote, ...)
        me-org            ; Org-mode for life (org-contrib, org-modern, org-appear, engrave-faces, citar, ...)
        me-prog           ; Programming stuff (simpc-mode, breadcrumb, hl-todo, xref-union, dumb-jump, apheleia, ...)
        me-project        ; Project management (otpp, projection, compile-multi, ...)
        ;; me-robot       ; Robotics stuff (ros, rosbag-info, ...)
        ;; me-rss         ; News feed (elfeed, elfeed-protocol, ...)
        me-services       ; Web services (jiralib, webpaste, isgd, igist, ...)
        me-snippets       ; Snippets (yasnippet, yasnippet-snippets, consult-yasnippet, ...)
        ;; me-tags        ; Source code tagging tools (citre, ggtags, call-graph, ...)
        me-tools          ; System tools (ecryptfs, ssh-deploy, envrc, pyenv, with-editor, ...)
        me-ui             ; User interface (doom-themes, page-break-lines, virtual-format, ...)
        me-vc))           ; Version control (magit, forge, diff-hl, git-timemachine, repo, diffview, ...)


;;; List of disabled packages
;; You can set `minemacs-disabled-packages' to disable some packages. For
;; example, if you want to use the `me-ui' module, but you want to disable the
;; `focus' package. You can use:
;; (push 'focus minemacs-disabled-packages)
;;
;; Adding a package to `minemacs-disabled-packages' guarantees disabling its
;; corresponding `use-package' section in MinEmacs' modules. However, please
;; note that, if you want to completely disable a package, you need to make sure
;; you've also disabled its dependent packages (see `M-x straight-dependents'),
;; otherwise it will get installed as a dependency.
;;
;; You can also `push' (or `add-to-list') multiple packages at once (as a list).
;; For example, to completely disable all of `magit', `magit-todos' and
;; `magit-file-icons', you can use:
;; (push '(magit magit-todos magit-file-icons) minemacs-disabled-packages)


;;; Immediately load the on-demand modules
;; MinEmacs includes a set of modules that are loaded on-demand. For instance,
;; when you open a "*.plantuml" file, the `on-demand/me-plantuml' module gets
;; automatically loaded. You can force MinEmacs to load the module by adding it
;; to your `minemacs-modules':
;; (push 'on-demand/me-plantuml minemacs-modules)


;;; Using the obsolete modules
;; You can use the obsolete packages configurations by adding the
;; `obsolete/me-*' modules to `minemacs-modules'. However, these modules, as
;; their names indicate, are OBSOLETE and NOT SUPPORTED. This is a
;; non-comprehensive list of obsolete modules, see "modules/obsolete/*.el" for
;; the full list.
;; (setq minemacs-modules
;;       (append
;;        minemacs-modules
;;        '(
;;          obsolete/me-blamer         ; M-x git blame
;;          obsolete/me-chezmoi        ; Integrate chezmoi with Emacs
;;          obsolete/me-cov            ; Show code coverage results (cov, ...)
;;          obsolete/me-eaf            ; EAF apps (browser, jupyter, file-sender, ...)
;;          obsolete/me-evil           ; Evil integration (evil, evil-snipe, general, ...)
;;          obsolete/me-meow           ; Yet another modal editing on Emacs (meow, ...)
;;          obsolete/me-expand-region  ; Expand region (included as an alternative for `expreg' in non tree-sitter builds)
;;          obsolete/me-flycheck       ; Static checkers (flycheck, ...)
;;          obsolete/me-flycheck-cmake ; Flycheck + CMake
;;          obsolete/me-flycheck-eglot ; Flycheck + Eglot
;;          obsolete/me-lexic          ; Offline dictionary using sdcv
;;          obsolete/me-ligature       ; Ligatures (needs further customization in function of the used font)
;;          obsolete/me-lsp            ; LSP and DAP (lsp-mode, dap-mode, consult-lsp, lsp-pyright, ccls, ...)
;;          obsolete/me-netextender    ; NetExtender integration (start/stop VPN sessions from Emacs)
;;          obsolete/me-org-present    ; Org presentations in Emacs
;;          obsolete/me-org-roam       ; Org roam configuration (org-roam, consult-org-roam, ...)
;;          obsolete/me-projectile     ; Project management (projectile, consult-projectile, treemacs-projectile, ...)
;;          obsolete/me-smartparens    ; Smartparens
;;          obsolete/me-spell-fu       ; Spell checking (included as an alternative when `jinx' cannot be used)
;;          obsolete/me-tree-sitter    ; Tree-sitter module configuration (this module is automatically activated for Emacs 28 or 29+ built without treesitter support)
;;          obsolete/me-unicode-fonts  ; Better Unicode management mainly for non-latin fonts
;;          obsolete/me-writeroom      ; Replacement for `+writing-mode' (writeroom-mode, ...)
;;          obsolete/me-yasnippet      ; Yasnippet (yasnippet, cape-yasnippet, yasnippet-snippets, ...)
;;         )))
