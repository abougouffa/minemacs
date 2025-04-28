;;; me-d.el --- D language support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-d
  :auto-mode '(("\\.d[i]?\\'" . d-mode)))


;; Major mode for the D programming language
(use-package d-mode
  :straight t)


(provide 'on-demand/me-d)
;;; me-d.el ends here
