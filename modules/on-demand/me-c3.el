;;; me-c3.el --- C3 language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@cneebg.pbz")
;; Created: 2025-08-11
;; Last modified: 2025-09-04

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-c3
  :auto-mode '(("\\.c3[it]?\\'" . c3-ts-mode)))


(use-package c3-ts-mode
  :straight (:host github :repo "c3lang/c3-ts-mode")
  :when (featurep 'feat/tree-sitter)
  :config
  (add-to-list 'treesit-language-source-alist '(c3 "https://github.com/c3lang/tree-sitter-c3"))
  (treesit-ensure-installed 'c3))


(provide 'on-demand/me-c3)
;;; me-c3.el ends here
