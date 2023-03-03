;;; me-modules.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Abdelhak Bougouffa

(defcustom minemacs-core-modules
  '(me-defaults      ;; Tweak Emacs defaults
    me-splash        ;; Simple splash screen (inspired by emacs-splash)
    me-bootstrap     ;; Bootstrap straight.el
    me-keybindings   ;; general.el, which-key, hydra, ...
    me-evil          ;; evil, evil-collection, evil-mc, ...
    me-core-ui       ;; Theme and modeline
    me-completion)  ;; vertico, marginalia, corfu, cape, consult, ...
  "MinEmacs enabled core modules. The order matters!")

(defcustom minemacs-modules
  '(me-ui            ;; User interface (focus, writeroom-mode, emojify, ...)
    me-editor        ;; Editing (yasnippet, smartparens, unicode-fonts, ligature, ...)
    me-extra         ;; Extra features (better-jumper, ...)
    me-undo          ;; Better undoing (undo-fu, undo-fu-session, vundo, ...)
    me-multi-cursors ;; Multi-cursors editing (iedit, evil-mc, ...)
    me-vc            ;; Version control (magit, forge, core-review, diff-hl, ...)
    me-project       ;; Project management (project, projectile, consult-projectile, treemacs-projectile, ...)
    me-prog          ;; Programming stuff (tree-sitter, eglot, eldoc, eldoc-box, apheleia, editorconfig, ...)
    me-checkers      ;; Static checkers (flymake, flymake-easy, ...)
    me-debug         ;; Debugging tools (gdb-mi, realgud, disaster, ...)
    ;; me-lsp        ;; LSP and DAP (lsp-mode, dap-mode, consult-lsp, lsp-pyright, ccls, ...)
    me-lisp          ;; Lisps development (parinfer-rust, macrostep, geiser, elisp, helpful, eros, ...)
    me-data          ;; Data file formats (csv, yaml, toml, json, ...)
    me-org           ;; Org-mode for life (org, org-contrib, org-modern, ...)
    me-notes         ;; Notes & Zettelkasten (org-roam, deft, ...)
    ;; me-email      ;; Email (mu4e, mu4e-alert, org-msg, ...)
    ;; me-lifestyle  ;; *Very* opinionated lifestyle packages (awqat, ...)
    me-docs          ;; Documents (pdf-tools, nov, ...)
    me-latex         ;; LaTeX (tex, auctex, reftex, ...)
    me-natural-langs ;; Natural language stuff (spell-fu, go-translate, eglot-ltex, ...)
    me-files         ;; Files and directories (dirvish, treemacs, vlf, ...)
    me-tools         ;; System tools (tramp, vterm, tldr, ssh-deploy, docker, ...)
    ;; me-biblio     ;; Bibliography & citations (org-cite, citar, zotxt, ...)
    me-daemon        ;; Emacs daemon tweaks
    me-tty           ;; Emacs from terminal (xt-mouse, xclip, ...)
    ;; me-rss        ;; News feed (elfeed, ...)
    ;; me-ros        ;; Robot Operating System (ros, ...)
    ;; me-embedded   ;; Embedded systems (arduino, openocd, bitbake, vhdl-mode, ...)
    me-eaf           ;; EAF apps (browser, jupyter, file-sender, ...)
    ;; me-math       ;; Mathematics (maxima, ess, ein, octave, ...)
    ;; me-modeling   ;; Modeling tools (scad-mode, ...)
    me-window        ;; Frame & window tweaks
    ;; me-media      ;; Multimedia (empv, ...)
    ;; me-fun        ;; Games and funny packages (xkcd, speed-type, ...)
    me-binary)       ;; Display binary files in hex or decompile them (hexl, ...) ...
  "MinEmacs enabled modules.")
