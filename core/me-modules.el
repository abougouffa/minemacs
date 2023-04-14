;;; me-modules.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Abdelhak Bougouffa

;;; Commentary:

;;; Code:

(defcustom minemacs-core-modules
  '(me-splash        ; Simple splash screen
    me-keybindings   ; Keybinding (general, which-key, hydra, ...)
    me-evil          ; Emacs as Vim (evil, evil-collection, evil-escape, evil-snipe, evil-numbers, ...)
    me-core-ui       ; Core UI (doom-themes, modus-themes, doom-modeline, ...)
    me-completion)   ; Completion (vertico, marginalia, corfu, cape, consult, embark, ...)
  "MinEmacs enabled core modules."
  :group 'minemacs-core
  :type '(repeat symbol))

(defcustom minemacs-modules
  '(me-ui            ; User interface (focus, writeroom-mode, mixed-pitch, ...)
    me-editor        ; Editing (yasnippet, smartparens, unicode-fonts, ligature, ...)
    me-daemon        ; Emacs daemon tweaks
    me-undo          ; Better undoing (undo-fu, undo-fu-session, vundo, ...)
    me-multi-cursors ; Multi-cursors editing (iedit, evil-mc, evil-iedit-state, ...)
    me-vc            ; Version control (magit, forge, core-review, diff-hl, ...)
    me-project       ; Project management (project, projectile, consult-projectile, treemacs-projectile, ...)
    me-prog          ; Programming stuff (tree-sitter, eglot, eldoc, eldoc-box, apheleia, editorconfig, ...)
    me-checkers      ; Static checkers (flymake, flymake-easy, ...)
    me-debug         ; Debugging tools (gdb-mi, realgud, disaster, ...)
    ;; me-lsp        ; LSP and DAP (lsp-mode, dap-mode, consult-lsp, lsp-pyright, ccls, ...)
    me-lisp          ; Lisps development (parinfer-rust, sly, macrostep, geiser, elisp, helpful, eros, ...)
    ;; me-embedded   ; Embedded systems (arduino, openocd, bitbake, vhdl-mode, ...)
    ;; me-robot      ; Robotics stuff (ros, robot-mode, ...)
    me-data          ; Data file formats (csv, yaml, toml, json, plantuml-mode, ...)
    ;; me-math       ; Mathematics (maxima, ess, ein, julia-mode, octave, ...)
    ;; me-modeling   ; Modeling tools (scad-mode, ...)
    me-org           ; Org-mode for life (org, org-contrib, org-modern, org-appear, ...)
    me-extra         ; Extra features (better-jumper, crux, ...)
    me-notes         ; Notes & Zettelkasten (org-roam, consult-org-roam, ...)
    me-eaf           ; EAF apps (browser, jupyter, file-sender, ...)
    ;; me-email      ; Email (mu4e, mu4e-alert, org-msg, ...)
    ;; me-rss        ; News feed (elfeed, ...)
    ;; me-lifestyle  ; *Very* opinionated lifestyle packages (awqat, ...)
    me-docs          ; Documents (pdf-tools, nov, ...)
    me-latex         ; LaTeX (auctex, auctex-latexmk, reftex, bibtex, ...)
    ;; me-biblio     ; Bibliography & citations (org-cite, citar, zotxt, ...)
    me-natural-langs ; Natural language stuff (spell-fu, go-translate, eglot-ltex, ...)
    me-files         ; Files and directories (dirvish, treemacs, vlf, ...)
    me-tools         ; System tools (tramp, vterm, tldr, ssh-deploy, docker, ...)
    me-tty           ; Emacs from terminal (xt-mouse, xclip, ...)
    ;; me-fun        ; Games and funny packages (xkcd, speed-type, ...)
    ;; me-media      ; Multimedia (empv, emms, ...)
    ;; me-workspaces ; Workspace separation (tabspaces, tab-bar, ...). NOTE: This is a WIP
    me-binary        ; Display binary files in hex or decompile them (hexl, ...) ...
    me-window)       ; Frame & window tweaks
  "MinEmacs enabled modules."
  :group 'minemacs-core
  :type '(repeat symbol))

;;; me-modules.el ends here
