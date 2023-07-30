;;; me-modules.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Abdelhak Bougouffa

;;; Commentary:

;;; Code:

(defcustom minemacs-core-modules
  '(me-splash
    me-keybindings
    me-evil
    me-core-ui
    me-completion)
  "MinEmacs enabled core modules."
  :group 'minemacs-core
  :type '(repeat symbol))

(defcustom minemacs-modules
  '(me-ui
    me-editor
    me-daemon
    me-undo
    me-multi-cursors
    me-vc
    me-project
    me-prog
    me-checkers
    ;; me-assistants
    me-debug
    ;; me-lsp
    me-emacs-lisp
    ;; me-common-lisp
    ;; me-scheme
    ;; me-clojure
    ;; me-embedded
    ;; me-robot
    me-data
    ;; me-math
    ;; me-modeling
    me-org
    me-extra
    me-notes
    me-eaf
    ;; me-email
    ;; me-rss
    ;; me-lifestyle
    me-docs
    ;; me-calendar
    me-latex
    ;; me-biblio
    me-natural-langs
    me-files
    me-tools
    me-tty
    ;; me-fun
    ;; me-media
    ;; me-workspaces
    me-binary
    me-window)
  "MinEmacs enabled modules."
  :group 'minemacs-core
  :type '(repeat symbol))

;;; me-modules.el ends here
