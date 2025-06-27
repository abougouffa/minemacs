;;; me-linux.el --- Linux kernel programming -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-06-27
;; Last modified: 2025-06-27

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-linux
  :auto-mode '(("/Kconfig\\'" . kconfig-mode)))


;; Major mode for editing Kconfig files
(use-package kconfig-mode
  :straight t
  :mode "/Kconfig\\'")


(provide 'on-demand/me-linux)
;;; me-linux.el ends here
