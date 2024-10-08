;;; me-modules.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 Abdelhak Bougouffa

;;; Commentary:

;;; Code:

(defcustom minemacs-modules
  '(;; me-ai
    ;; me-biblio
    ;; me-calendar
    me-checkers
    me-completion
    me-daemon
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
    ;; me-lifestyle
    ;; me-math
    ;; me-media
    me-multi-cursors
    me-natural-langs
    me-notes
    me-org
    me-prog
    me-project
    ;; me-robot
    ;; me-rss
    me-search
    ;; me-services
    ;; me-tags
    me-tools
    me-tty
    me-ui
    me-vc
    me-window)
  "MinEmacs enabled modules."
  :group 'minemacs-core
  :type '(repeat symbol))

;;; me-modules.el ends here
