;;; modules.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Abdelhak Bougouffa

;; This file can be used to override `minemacs-modules'
;; and `minemacs-core-modules'

;; Ordered list of enabled core modules
(setq minemacs-core-modules
      '(me-splash        ; Simple splash screen
        me-keybindings   ; Keybinding (general, which-key, hydra, ...)
        me-evil          ; Emacs as Vim (evil, evil-collection, evil-escape, evil-snipe, evil-numbers, ...)
        me-core-ui       ; Core UI (doom-themes, modus-themes, doom-modeline, ...)
        me-completion))  ; Completion (vertico, marginalia, corfu, cape, consult, embark, ...)

;; List of enabled modules
(setq minemacs-modules
      '(me-ui            ; User interface (focus, writeroom-mode, mixed-pitch, ...)
        me-editor        ; Editing (tempel, smartparens, unicode-fonts, ligature, ...)
        me-daemon        ; Emacs daemon tweaks
        me-undo          ; Better undoing (undo-fu, undo-fu-session, vundo, ...)
        me-multi-cursors ; Multi-cursors editing (iedit, evil-mc, evil-iedit-state, ...)
        me-vc            ; Version control (magit, forge, core-review, diff-hl, ...)
        me-project       ; Project management (project, consult-project-extra, ...)
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
        me-fun           ; Games and funny packages (xkcd, speed-type, ...)
        me-media         ; Multimedia (empv, emms, ...)
        ;; me-workspaces ; Workspace separation (tabspaces, tab-bar, ...)
        me-binary        ; Display binary files in hex or decompile them (hexl, ...) ...
        me-window))      ; Frame & window tweaks

;; You can set `minemacs-disabled-packages' to disable some packages. For
;; example, if you want to use the `me-ui' module, but you want to disable the
;; `focus' package. You can use:
(push 'focus minemacs-disabled-packages)

;; You can use the obsolete configurations by adding the `obsolete/me-*' modules to `minemacs-modules'
;; (setq minemacs-modules
;;       (append
;;        minemacs-modules
;;        '(obsolete/me-yasnippet   ; Yasnippet (yasnippet, cape-yasnippet, yasnippet-snippets, ...)
;;          obsolete/me-writeroom   ; Replacement for `+writing-mode' (writeroom-mode, ...)
;;          obsolete/me-projectile  ; Project management (projectile, consult-projectile, treemacs-projectile, ...)
;;          obsolete/me-flycheck))) ; Static checkers (flycheck, ...)
