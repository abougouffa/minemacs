;;; me-nav.el --- MinEmacs search & navigation packages -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;; Jump to things in Emacs tree-style
(use-package avy
  :straight t
  :bind (("M-j" . avy-goto-char-timer) ; Instead of `default-indent-new-line'
         ("C-é" . avy-goto-line) ; French AZERTY
         ("M-g l" . avy-goto-line)))


;; Zap to char using `avy'
(use-package avy-zap
  :straight t
  :bind ("M-z" . avy-zap-to-char-dwim))


;; Jump around your source code in emacs using `treesit' and `avy'
(use-package treesit-jump
  :straight (:host github :repo "abougouffa/treesit-jump" :branch "enhancements" :files (:defaults "treesit-queries"))
  :when (+emacs-options-p 'tree-sitter))


;; Never lose your place in Emacs again
(use-package dogears
  :straight t
  :hook (minemacs-lazy . dogears-mode)
  :bind (("M-g d"   . dogears-go)
         ("M-g M-b" . dogears-back)
         ("M-g M-f" . dogears-forward)
         ("M-g M-d" . dogears-list)
         ("M-g M-D" . dogears-sidebar)
         ([mouse-8] . dogears-back)
         ([mouse-9] . dogears-forward))
  :custom
  (dogears-hooks '(imenu-after-jump-hook xref-after-jump-hook xref-after-return-hook consult-after-jump-hook rtags-jump-hook)))


;; An Emacs package to move point through `buffer-undo-list' positions
(use-package goto-last-change
  :bind (("M-é" . goto-last-change))) ; For French AZERTY keybords


;; Extensions to `isearch'
(use-package isearch+
  :straight t)


;; Another incremental search command, compatible with `multiple-cursors'
(use-package phi-search
  :straight t)


;; Emacs search tool based on "ripgrep"
(use-package rg
  :straight t
  :bind (("C-c s" . rg-menu)))


;; Asynchronous fuzzy finder for Emacs
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


;; An Emacs front-end for "fzf"
(use-package fzf
  :straight t
  :commands (+fzf-project-super-project)
  :init
  (defalias '+fzf-project 'fzf-projectile)
  :config
  ;; Define a super-project variant of `+fzf-project'
  (+super-project-define-commands 'fzf '+fzf-project))


(provide 'me-nav)
;;; me-nav.el ends here
