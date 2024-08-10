;;; me-modelica.el --- Modelica -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-extra-mode 'me-modelica 'modelica-mode
  :auto-mode '(("\\.mo\\'" . modelica-mode)))

(use-package modelica-mode
  :straight t
  :mode "\\.mo\\'")


(provide 'modes/me-modelica)
;;; me-modelica.el ends here
