;;; me-search.el --- MinEmacs search & navigation packages -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn-rkg@fntrzpbz.pbz")

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

(use-package loccur
  :straight t
  :bind (("C-S-o" . loccur-current)
         ("C-M-S-o" . loccur)
         ("M-s C-o" . loccur-isearch)
         :map isearch-mode-map
         ("C-o" . loccur-isearch)))

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
  :commands +fzf-project +fzf-super-project
  :config
  (defalias '+fzf-project 'fzf-projectile)
  ;; fzf.el relays on `projectile-project-root' to guess the project root
  (unless (fboundp 'projectile-project-root)
    (provide 'projectile) ; provide `projectile' because `fzf-projectile' will try to require it
    (defalias 'projectile-project-root (lambda () (ignore-errors (project-root (project-current))))))

  ;; Run fzf in the workspace (super-project with multiple projects inside)
  (defvar +fzf-super-project-root-markers '(".super-project" ".super-project.el" ".repo" ".code-workspace" ".workspace")
    "List of super-projects root markers, order matters.")

  (defun +fzf-super-project (&optional with-preview)
    "Starts an fzf session at the root of the current super-project (workspace)."
    (interactive "P")
    (if-let* ((starting-path (or (when-let ((proj (project-current))) (project-root proj))
                                 buffer-file-name
                                 default-directory))
              (repo-dir (cl-some (apply-partially #'locate-dominating-file starting-path) +fzf-super-project-root-markers)))
        (let ((fzf/args (if with-preview
                            (concat fzf/args " " fzf/args-for-preview)
                          fzf/args))
              (fzf--target-validator (fzf--use-validator #'fzf--validate-filename)))
          (fzf--start repo-dir #'fzf--action-find-file))
      (user-error "It doesn't seem that we are in a super-project"))))


(provide 'me-search)
;;; me-search.el ends here
