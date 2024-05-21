;;; me-modules.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Abdelhak Bougouffa

;;; Commentary:

;;; Code:

(defcustom minemacs-core-modules
  '(me-completion
    me-core-ui
    me-evil
    me-keybindings
    me-splash)
  "MinEmacs enabled core modules."
  :group 'minemacs-core
  :type '(repeat symbol))

(defcustom minemacs-modules
  '(
    ;; me-ai
    ;; me-biblio
    ;; me-binary
    ;; me-calendar
    me-checkers
    ;; me-clojure
    ;; me-common-lisp
    me-daemon
    me-data
    me-debug
    me-docs
    me-editor
    me-emacs-lisp
    ;; me-email
    ;; me-embedded
    me-extra
    me-files
    me-formal
    ;; me-fun
    me-latex
    ;; me-lifestyle
    ;; me-math
    ;; me-media
    ;; me-modeling
    me-multi-cursors
    ;; me-nano
    me-natural-langs
    me-notes
    me-org
    me-prog
    me-project
    ;; me-robot
    ;; me-rss
    ;; me-scheme
    ;; me-tags
    me-tools
    me-tty
    me-ui
    me-undo
    me-vc
    me-window
    me-workspaces)
  "MinEmacs enabled modules."
  :group 'minemacs-core
  :type '(repeat symbol))

;;; me-modules.el ends here
