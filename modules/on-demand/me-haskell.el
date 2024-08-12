;;; me-haskell.el --- Haskell language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn-rkg@fntrzpbz.pbz")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-haskell
  :auto-mode '((("\\.hsig\\'" "\\.[gh]s\\'" "\\.hsc\\'") . haskell-mode) ("\\.l[gh]s\\'" . haskell-literate-mode))
  :interpreter-mode '(("runghc" . haskell-mode) ("runhaskell" . haskell-mode)))

(use-package haskell-mode
  :straight t)


(provide 'on-demand/me-haskell)
;;; me-haskell.el ends here
