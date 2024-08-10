;;; me-demangle.el --- demangle-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-extra-mode 'me-demangle 'demangle-mode
  :companion-packages '(((llvm-mode llvm-ts-mode) . demangle-mode)))

(use-package demangle-mode
  :straight t)


(provide 'modes/me-demangle)
;;; me-demangle.el ends here
