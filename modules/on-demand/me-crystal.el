;;; me-crystal.el --- Crystal language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-crystal
  :auto-mode '(("\\(?:\\.cr\\)\\'" . crystal-mode))
  :interpreter-mode '(("crystal" . crystal-mode)))

(use-package crystal-mode
  :straight t)


(provide 'on-demand/me-crystal)
;;; me-crystal.el ends here
