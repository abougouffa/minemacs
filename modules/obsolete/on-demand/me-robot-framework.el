;;; me-robot-framework.el --- Robot Framework support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-10
;; Last modified: 2026-03-29

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-robot-framework
  :auto-mode '(("\\.\\(robot\\|resource\\)\\'" . robot-mode)))


;; Major mode for Robot Framework files
(use-package robot-mode
  :straight t)


(provide 'on-demand/me-robot-framework)
;;; me-robot-framework.el ends here
