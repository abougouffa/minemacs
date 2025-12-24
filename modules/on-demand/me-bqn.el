;;; me-bqn.el --- BQN language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-22
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-bqn
  :auto-mode '(("\\.bqn\\'" . bqn-mode))
  :interpreter-mode '(("bqn" . bqn-mode)))


;; Major mode for editing BQN grammar files
(use-package bqn-mode
  :ensure t)


(provide 'on-demand/me-bqn)
;;; me-bqn.el ends here
