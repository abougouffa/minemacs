;;; me-yasnippet.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@cneebg.pbz")
;; Created: 2025-07-29
;; Last modified: 2026-06-01

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
    (+add-advice '(yas-define-snippets yas--parse-template) :around '+apply-inhibit-messages))
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


;; Completion-At-Point Extension for YASnippet
(use-package yasnippet-capf
  :straight t
  :bind (("C-c p y" . yasnippet-capf))
  :init
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))


(provide 'obsolete/me-yasnippet)
;;; me-yasnippet.el ends here
