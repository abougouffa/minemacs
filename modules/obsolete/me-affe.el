;;; me-affe.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@tznvy.pbz")
;; Created: 2026-05-04
;; Last modified: 2026-05-04

;;; Commentary:

;;; Code:


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


(provide 'obsolete/me-affe)
;;; me-affe.el ends here
