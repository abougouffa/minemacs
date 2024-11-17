;;; me-python.el --- Python extra packages -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-python
  :companion-packages '(((python-mode python-ts-mode) . python-docstring-mode)))


;; Smart Python docstring formatting
(use-package python-docstring
  :straight t
  :hook ((python-mode python-ts-mode) . python-docstring-mode))


;; Helpers to run Python's pytest
(use-package python-pytest
  :straight t)


(provide 'on-demand/me-python)
;;; me-python.el ends here
