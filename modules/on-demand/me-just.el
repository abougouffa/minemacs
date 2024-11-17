;;; me-just.el --- Justfile support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

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
  :straight t)


(provide 'on-demand/me-just)
;;; me-just.el ends here
