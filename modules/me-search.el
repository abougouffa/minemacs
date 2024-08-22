;;; me-search.el --- MinEmacs search & navigation packages -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;; TODO: https://karthinks.com/software/avy-can-do-anything
(use-package avy
  :straight t
  :bind (("M-j" . avy-goto-char-timer) ; Instead of `default-indent-new-line'
         ("C-é" . avy-goto-line) ; French AZERTY
         ("M-g l" . avy-goto-line)))

(use-package avy-zap
  :straight t
  :bind ("M-z" . avy-zap-to-char-dwim))

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

(use-package goto-last-change
  :bind (("M-é" . goto-last-change))) ; For French AZERTY keybords

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

(use-package affe
  :straight t
  :custom
  (affe-regexp-compiler #'+affe-orderless-regexp-compiler)
  :config
  ;; Setup orderless regexp compiler, as recommended in the README.md
  (require 'orderless)
  (defun +affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (cdr (orderless-compile input)))
    (cons input (apply-partially #'orderless--highlight input t))))

(use-package fzf
  :straight t
  :commands (+fzf-project-super-project)
  :init
  (defalias '+fzf-project 'fzf-projectile)
  :config
  ;; Define a super-project variant of `+fzf-project'
  (+super-project-define-commands 'fzf '+fzf-project))


(provide 'me-search)
;;; me-search.el ends here
