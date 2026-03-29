;;; me-rtf.el --- Rich Text Files -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2026-03-29
;; Last modified: 2026-03-29

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-rtf
  :auto-mode '(("\\.rtf\\'" . rtf-view)))


;; View Rich Text Format (.rtf) files
(use-package rtf-view
  :straight t
  :when (and (libxml-available-p) (executable-find "unrtf")))


(provide 'on-demand/me-rtf)
;;; me-rtf.el ends here
