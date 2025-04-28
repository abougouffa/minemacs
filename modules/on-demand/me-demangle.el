;;; me-demangle.el --- demangle-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-11
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-demangle
  :companion-packages '(((llvm-mode llvm-ts-mode) . demangle-mode)))


;; Automatically demangle C++, D, and Rust symbols in LLVM code
(use-package demangle-mode
  :straight t)


(provide 'on-demand/me-demangle)
;;; me-demangle.el ends here
