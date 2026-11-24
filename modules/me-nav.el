;;; me-nav.el --- MinEmacs search & navigation packages -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-06-14
;; Last modified: 2026-11-25

;;; Commentary:

;;; Code:

;; Quickly switch windows in Emacs
(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window))
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))


;; Jump to things in Emacs tree-style
(use-package avy
  :ensure t
  :bind (("M-j" . avy-goto-char-timer) ; Instead of `default-indent-new-line'
         ("C-é" . avy-goto-line) ; French AZERTY
         ("M-g l" . avy-goto-line)))


;; Zap to char using `avy'
(use-package avy-zap
  :ensure t
  :bind ("M-z" . avy-zap-to-char-dwim))


;; Never lose your place in Emacs again
(use-package dogears
  :ensure t
  :hook (minemacs-lazy . dogears-mode)
  :bind (("M-g d"   . dogears-go)
         ("M-g M-b" . dogears-back)
         ("M-g M-f" . dogears-forward)
         ("M-g M-d" . dogears-list)
         ("M-g M-D" . dogears-sidebar)
         ([mouse-8] . dogears-back)
         ([mouse-9] . dogears-forward))
  :custom
  (dogears-hooks '(imenu-after-jump-hook xref-after-jump-hook xref-after-return-hook consult-after-jump-hook dumb-jump-after-jump-hook rtags-jump-hook)))


;; Move to the beginning/end of line, code or comment
(use-package mwim
  :ensure t
  :bind (("C-a" . mwim-beginning) ; Instead of `move-beginning-of-line' or `beginning-of-visual-line'
         ("C-e" . mwim-end))) ; Instead of `move-end-of-line' or `end-of-visual-line'


;; Go to last change
(use-package goto-chg
  :ensure t
  :bind (("M-é" . goto-last-change) ; For French AZERTY keybords
         ("C-M-é" . goto-last-change-reverse)))


;; Another incremental search command, compatible with `multiple-cursors'
(use-package phi-search
  :ensure t)


;; Emacs search tool based on "ripgrep"
(use-package rg
  :ensure t
  :bind (("C-c s" . rg-menu))
  :init
  (+super-project-define-commands 'rg-project))


;; Emacs search engine which combines concepts from information retrievial and Bayesian search theory
(use-package p-search
  :ensure t
  :commands (p-search))


;; An Emacs front-end for "fzf"
(use-package fzf
  :ensure t
  :commands (+fzf-project-super-project)
  :init
  (defalias '+fzf-project 'fzf-projectile)
  (+super-project-define-commands '+fzf-project)) ; Define a super-project variant of `+fzf-project'


(provide 'me-nav)

;;; me-nav.el ends here
