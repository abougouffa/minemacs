;;; me-search.el --- MinEmacs search & navigation packages -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn-rkg@fntrzpbz.pbz")

;;; Commentary:

;;; Code:

;; TODO: https://karthinks.com/software/avy-can-do-anything
(use-package avy
  :straight t
  :bind (("C-\"" . avy-goto-char)
         ("C-é" . avy-goto-line) ; French AZERTY
         ("M-g l" . avy-goto-line)
         ("M-j" . avy-goto-char-timer))) ; Instead of `default-indent-new-line'

(use-package dogears
  :straight t
  :hook (minemacs-lazy . dogears-mode)
  :bind (("M-g d" . dogears-go)
         ("M-g M-b" . dogears-back)
         ("M-g M-f" . dogears-forward)
         ("M-g M-d" . dogears-list)
         ("M-g M-D" . dogears-sidebar)
         ("<mouse-8>" . dogears-back)
         ("<mouse-9>" . dogears-forward))
  :custom
  (dogears-hooks '(imenu-after-jump-hook xref-after-jump-hook xref-after-return-hook consult-after-jump-hook rtags-jump-hook)))

(use-package isearch+
  :straight t)

(use-package isearch-mb
  :straight t
  :after isearch
  :init
  (isearch-mb-mode 1))

(use-package phi-search
  :straight t)

(use-package phi-grep
  :straight t)

(use-package rg
  :straight t
  :bind (("C-c s" . rg-menu)))

(use-package fzf
  :straight t
  :commands fzf-project
  :config
  (defalias 'fzf-project 'fzf-projectile)
  ;; fzf.el relays on `projectile-project-root' to guess the project root
  (unless (fboundp 'projectile-project-root)
    (provide 'projectile) ; provide `projectile' because `fzf-projectile' will try to require it
    (defalias 'projectile-project-root (lambda () (ignore-errors (project-root (project-current)))))))


(provide 'me-search)
;;; me-search.el ends here
