;;; me-just.el --- Justfile support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-just
  :auto-mode '((("/[Jj]ustfile\\'" "\\.[Jj]ust\\(file\\)?\\'") . just-mode)))

(use-package just-mode
  :straight t)


(provide 'on-demand/me-just)
;;; me-just.el ends here
