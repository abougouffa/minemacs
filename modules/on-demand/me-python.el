;;; me-python.el --- Python extra packages -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-extra-mode 'me-python 'python-docstring
  :companion-packages '(((python-mode python-ts-mode) . python-docstring-mode)))

(use-package python-docstring
  :straight t
  :hook ((python-mode python-ts-mode) . python-docstring-mode))


(provide 'on-demand/me-python)
;;; me-python.el ends here
