;;; me-linux.el --- Linux kernel programming -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-06-27
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-linux
  :auto-mode '(("/Kconfig\\'" . kconfig-mode)
               ("\\.cocci$" . cocci-mode)
               ("\\.iso$" . cocci-mode)))


;; Major mode for editing Kconfig files
(use-package kconfig-mode
  :ensure t
  :mode "/Kconfig\\'")


;; Coccinelle: Complex style-preserving source-to-source transformations
(use-package cocci
  :when (file-exists-p "/usr/share/emacs/site-lisp/cocci.el")
  :load-path "/usr/share/emacs/site-lisp/"
  :mode ("\\.iso$" . cocci-mode)
  :mode ("\\.cocci$" . cocci-mode))


(provide 'on-demand/me-linux)
;;; me-linux.el ends here
