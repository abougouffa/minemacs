;;; me-mode-framework.el --- Robot Framework support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-10
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-mode-framework
  :auto-mode '(("\\.\\(robot\\|resource\\)\\'" . robot-mode)))


;; Major mode for Robot Framework files
(use-package robot-mode
  :straight t)


(provide 'on-demand/me-mode-framework)
;;; me-mode-framework.el ends here
