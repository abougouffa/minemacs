;;; me-d.el --- D language support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-d
  :auto-mode '(("\\.d[i]?\\'" . d-mode)))

(use-package d-mode
  :straight t)


(provide 'on-demand/me-d)
;;; me-d.el ends here
