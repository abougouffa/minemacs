;; modules.el --- Enable all modules in CI mode -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(setq minemacs-core-modules
      '(me-splash me-keybindings me-evil me-core-ui me-completion)
      minemacs-modules
      '(me-ui me-editor me-daemon me-undo me-multi-cursors me-vc me-project
        me-prog me-checkers me-debug me-lsp me-emacs-lisp me-common-lisp
        me-scheme me-clojure me-embedded me-robot me-data me-math me-modeling
        me-org me-extra me-notes me-email me-rss me-lifestyle me-docs me-calendar
        me-latex me-biblio me-natural-langs me-files me-tools me-tty me-fun
        me-media me-workspaces me-binary me-window))
