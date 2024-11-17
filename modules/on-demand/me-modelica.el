;;; me-modelica.el --- Modelica -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-modelica
  :auto-mode '(("\\.mo\\'" . modelica-mode)))


;; Major mode for editing Modelica files
(use-package modelica-mode
  :straight t
  :mode "\\.mo\\'")


(provide 'on-demand/me-modelica)
;;; me-modelica.el ends here
