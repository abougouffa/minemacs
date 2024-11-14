;;; me-awk.el --- More AWK stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-awk
  :auto-mode '(("\\.awk\\'" . awk-ts-mode))
  :companion-packages '((awk-mode . awk-ts-mode)))

(use-package awk-ts-mode
  :straight t
  :when (+emacs-options-p 'tree-sitter))


(provide 'on-demand/me-awk)
;;; me-awk.el ends here
