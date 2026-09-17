;;; me-just.el --- Justfile support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-10
;; Last modified: 2026-09-17

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-just
  :auto-mode '((("/[Jj]ustfile\\'" "\\.[Jj]ust\\(file\\)?\\'") . just-mode))
  :companion-packages '(((just-mode just-ts-mode) . justl)))


;; Major mode for editing Justfile
(use-package just-mode
  :straight t)


;; Major mode for driving just files
(use-package justl
  :straight t
  :custom
  (justl-shell (cond ((require 'ghostel nil t) 'ghostel)
                     ((require 'eat nil t) 'eat)
                     ((require 'vterm nil t) 'vterm)
                     (t 'eshell))))


(provide 'on-demand/me-just)
;;; me-just.el ends here
