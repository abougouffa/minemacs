;;; me-modules.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 Abdelhak Bougouffa

;;; Commentary:

;;; Code:

(defcustom minemacs-modules
  '(;; me-ai
    ;; me-biblio
    ;; me-calendar
    me-checkers
    ;; me-clojure
    ;; me-common-lisp
    me-completion
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
    ;; me-fun
    me-god
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
