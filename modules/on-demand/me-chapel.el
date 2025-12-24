;;; me-chapel.el --- Chapel lanugage -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-22
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-chapel
  :auto-mode '(("\\.chpl\\'" . chapel-mode)))


;; A major mode for the Chapel programming language
(use-package chapel-mode
  :ensure t)


(provide 'on-demand/me-chapel)
;;; me-chapel.el ends here
