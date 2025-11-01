;;; me-snippets.el --- Snippets -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-11-17
;; Last modified: 2025-07-29

;;; Commentary:

;;; Code:


;; A template system for Emacs
(use-package yasnippet
  :straight t
  :hook ((text-mode prog-mode conf-mode) . yas-minor-mode)
  :init
  (defvar yas-verbosity (if minemacs-verbose-p 4 2))
  (unless minemacs-verbose-p
    ;; Suppress some annoying messages like: "Multiple snippets with same identity: ...", "Ignoring unknown directive ..."
    (satch-advice-add '(yas-define-snippets yas--parse-template) :around '+apply-inhibit-messages))
  :custom
  (yas-triggers-in-field t) ; Allow nested snippets
  (yas-alias-to-yas/prefix-p nil) ; Don't define old `yas/*' aliases
  (yas-snippet-dirs (list (+directory-ensure minemacs-config-dir "snippets/") (concat minemacs-root-dir "snippets/"))))


;; A collection of yasnippet snippets for many languages
(use-package yasnippet-snippets
  :straight t)


;; A consulting-read interface for yasnippet
(use-package consult-yasnippet
  :straight t
  :unless (+package-disabled-p 'consult 'me-completion))


(provide 'me-snippets)
;;; me-snippets.el ends here
