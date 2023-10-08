;;; modules.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Abdelhak Bougouffa

;; This file can be used to override `minemacs-modules'
;; and `minemacs-core-modules'

;;; Ordered list of enabled core modules
(setq minemacs-core-modules
      '(me-splash        ; Simple splash screen
        me-keybindings   ; Keybinding (general, which-key, hydra, ...)
        me-evil          ; Emacs as Vim (evil, evil-collection, evil-escape, evil-snipe, evil-numbers, ...)
        me-core-ui       ; Core UI (doom-themes, modus-themes, doom-modeline, ...)
        me-completion))  ; Completion (vertico, marginalia, corfu, cape, consult, embark, ...)

;;; List of enabled modules
(setq minemacs-modules
      '(me-ui             ; User interface (svg-lib, focus, mixed-pitch, ...)
        me-editor         ; Editing (tempel, smartparens, ligature, ...)
        me-daemon         ; Emacs daemon tweaks
        me-undo           ; Better undoing (undo-fu, undo-fu-session, vundo, ...)
        me-multi-cursors  ; Multi-cursors editing (iedit, evil-mc, evil-iedit-state, ...)
        me-vc             ; Version control (magit, forge, core-review, diff-hl, ...)
        me-project        ; Project management (consult-project-extra, ibuffer-project, ...)
        me-prog           ; Programming stuff (tree-sitter, eldoc-box, apheleia, editorconfig, ...)
        me-checkers       ; Static checkers (flymake-easy, ...)
        me-debug          ; Debugging tools (realgud, disaster, ...)
        ;; me-lsp         ; LSP and DAP (lsp-mode, dap-mode, consult-lsp, lsp-pyright, ccls, ...)
        me-emacs-lisp     ; Emacs lisp development (parinfer-rust, macrostep, eros, helpful, ...)
        ;; me-common-lisp ; Common Lisp development (sly, sly-quicklisp, ...)
        ;; me-scheme      ; Scheme development (racket-mode, geiser, ...)
        ;; me-clojure     ; Clojure development (clojure-mode, cider, ...)
        ;; me-embedded    ; Embedded systems (arduino, openocd, bitbake, ...)
        ;; me-robot       ; Robotics stuff (ros, robot-mode, ...)
        me-data           ; Data file formats (csv, yaml, toml, json, plantuml-mode, ...)
        ;; me-math        ; Mathematics (maxima, ess, ein, julia-mode, ...)
        ;; me-modeling    ; Modeling tools (scad-mode, ...)
        me-org            ; Org-mode for life (org-contrib, org-modern, org-appear, ...)
        me-extra          ; Extra features (better-jumper, crux, ...)
        me-notes          ; Notes & Zettelkasten (denote, ...)
        me-eaf            ; EAF apps (browser, jupyter, file-sender, ...)
        ;; me-email       ; Email (mu4e, mu4e-alert, org-msg, ...)
        ;; me-rss         ; News feed (elfeed, ...)
        ;; me-lifestyle   ; *Very* opinionated lifestyle packages (awqat, ...)
        me-docs           ; Documents (pdf-tools, nov, ...)
        ;; me-calendar    ; Calendar (calfw, calfw-org, calfw-ical, ...)
        me-latex          ; LaTeX (auctex, auctex-latexmk, ...)
        ;; me-biblio      ; Bibliography & citations (citar, zotxt, ...)
        me-natural-langs  ; Natural language stuff (spell-fu, go-translate, eglot-ltex, ...)
        me-files          ; Files and directories (dirvish, treemacs, vlf, ...)
        me-tools          ; System tools (vterm, tldr, ssh-deploy, docker, ...)
        me-tty            ; Emacs from terminal (xt-mouse, xclip, ...)
        me-fun            ; Games and funny packages (xkcd, speed-type, ...)
        me-media          ; Multimedia (empv, emms, ...)
        ;; me-workspaces  ; Workspace separation (tabspaces, ...)
        me-binary         ; Display binary files in hex or decompile them
        me-window))       ; Frame & window tweaks

;;; List of disabled packages
;; You can set `minemacs-disabled-packages' to disable some packages. For
;; example, if you want to use the `me-ui' module, but you want to disable the
;; `focus' package. You can use:
;; (push 'focus minemacs-disabled-packages)

;; Adding a package to `minemacs-disabled-packages' guarantees disabling its
;; corresponding `use-package' section in MinEmacs' modules. However, please
;; note that, if you want to completely disable a package, you need to make sure
;; you've also disabled its dependent packages (see `M-x straight-dependents'),
;; otherwise it will get installed as a dependency.
;;
;; You can also `push' (or `add-to-list') multiple packages at once (as a list).
;; For example, to completely disable `iedit' and its dependencies
;; `evil-multiedit' and `evil-iedit-state', you can use:
;; (push '(iedit evil-multiedit evil-iedit-state) minemacs-disabled-packages)

;;; Using the obsolete modules
;; You can use the obsolete packages configurations by adding the
;; `obsolete/me-*' modules to `minemacs-modules'. However, these modules, as
;; their names indicate, are OBSOLETE and NOT SUPPORTED.
;; (setq minemacs-modules
;;       (append
;;        minemacs-modules
;;        '(obsolete/me-yasnippet     ; Yasnippet (yasnippet, cape-yasnippet, yasnippet-snippets, ...)
;;          obsolete/me-tree-sitter   ; Tree-sitter module configuration (this module is automatically activated for Emacs 28 or 29+ built without treesitter support)
;;          obsolete/me-org-roam      ; Org roam configuration (org-roam, consult-org-roam, ...)
;;          obsolete/me-cov           ; Show code coverage results (cov, ...)
;;          obsolete/me-writeroom     ; Replacement for `+writing-mode' (writeroom-mode, ...)
;;          obsolete/me-projectile    ; Project management (projectile, consult-projectile, treemacs-projectile, ...)
;;          obsolete/me-unicode-fonts ; Better Unicode management mainly for non-latin fonts
;;          obsolete/me-flycheck)))   ; Static checkers (flycheck, ...)
