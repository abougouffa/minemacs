;;; me-modules.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Abdelhak Bougouffa

;;; Commentary:

;;; Code:

(defcustom minemacs-core-modules '()
  "MinEmacs enabled core modules."
  :group 'minemacs-core
  :type '(repeat symbol))

(make-obsolete-variable 'minemacs-core-modules 'minemacs-modules "MinEmacs v7.0.0")

(defcustom minemacs-modules
  '(;; me-ai
    ;; me-biblio
    ;; me-binary
    ;; me-calendar
    me-checkers
    ;; me-clojure
    ;; me-common-lisp
    me-completion
    me-core-ui
    me-daemon
    me-data
    me-debug
    me-docs
    me-editor
    me-emacs-lisp
    ;; me-email
    ;; me-embedded
    me-evil
    me-extra
    me-files
    ;; me-fun
    ;; me-gtd
    me-keybindings
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
    ;; me-services
    me-splash
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
