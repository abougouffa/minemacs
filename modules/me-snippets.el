;;; me-snippets.el --- Snippets -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:


;; A template system for Emacs
(use-package yasnippet
  :ensure t
  :hook ((text-mode prog-mode conf-mode) . yas-minor-mode)
  :init
  (defvar yas-verbosity (if minemacs-verbose-p 4 2))
  (unless minemacs-verbose-p
    ;; Suppress some annoying messages like: "Multiple snippets with same identity: ...", "Ignoring unknown directive ..."
    (satch-advice-add '(yas-define-snippets yas--parse-template) :around '+apply-inhibit-messages))
  :custom
  (yas-triggers-in-field t) ; Allow nested snippets
  (yas-snippet-dirs (list (+directory-ensure minemacs-config-dir "snippets/") (concat minemacs-root-dir "snippets/"))))


;; Completion-At-Point Extension for YASnippet
(use-package yasnippet-capf
  :ensure t
  :hook ((prog-mode text-mode conf-mode) . +cape-yasnippet--setup-h)
  :bind (("C-c p y" . yasnippet-capf))
  :init
  (defun +cape-yasnippet--setup-h ()
    (when (bound-and-true-p yas-minor-mode)
      (add-to-list 'completion-at-point-functions #'yasnippet-capf))))


;; A collection of yasnippet snippets for many languages
(use-package yasnippet-snippets
  :ensure t)


(provide 'me-snippets)
;;; me-snippets.el ends here
