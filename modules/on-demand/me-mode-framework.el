;;; me-mode-framework.el --- Robot Framework support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-mode-framework
  :auto-mode '(("\\.\\(robot\\|resource\\)\\'" . robot-mode)))

(use-package robot-mode
  :straight t)


(provide 'on-demand/me-mode-framework)
;;; me-mode-framework.el ends here
