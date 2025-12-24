;;; me-just.el --- Justfile support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-10
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-just
  :auto-mode '((("/[Jj]ustfile\\'" "\\.[Jj]ust\\(file\\)?\\'") . just-mode))
  :companion-packages '(((just-mode just-ts-mode) . justl)))


;; Major mode for editing Justfile
(use-package just-mode
  :ensure t)


;; Major mode for driving just files
(use-package justl
  :ensure t)


(provide 'on-demand/me-just)
;;; me-just.el ends here
