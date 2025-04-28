;;; me-chapel.el --- Chapel lanugage -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-chapel
  :auto-mode '(("\\.chpl\\'" . chapel-mode)))


;; A major mode for the Chapel programming language
(use-package chapel-mode
  :straight t)


(provide 'on-demand/me-chapel)
;;; me-chapel.el ends here
