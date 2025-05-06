;;; me-boogie.el --- A collection of tools for interacting with Boogie and related languages -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@cneebg.pbz")
;; Created: 2025-05-06
;; Last modified: 2025-05-06

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-boogie
  :auto-mode '(("\\.dfy\\'" . dafny-mode)
               ("\\.bpl\\'" . boogie-mode)
               ("\\.smt2\\'" . z3-smt2-mode)))


(use-package boogie-friends
  :straight t)


(provide 'on-demand/me-boogie)
;;; me-boogie.el ends here
