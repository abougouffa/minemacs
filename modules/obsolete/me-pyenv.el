;;; me-pyenv.el --- Python environment integration (replaced with `pet') -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package pyvenv
  :straight t)

(use-package pyenv
  :straight (:host github :repo "aiguofer/pyenv.el")
  :hook (minemacs-first-python-file . +global-pyenv-mode-maybe)
  :custom
  (pyenv-show-active-python-in-modeline nil)
  :config
  (defun +global-pyenv-mode-maybe (&optional arg)
    "Enable `pyenv-global-mode' if it can be enabled."
    (interactive (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg) 'toggle)))
    (if (file-executable-p pyenv-executable)
        (global-pyenv-mode arg)
      (+log! "The %S file doesn't exist or is not executable, `pyenv' cannot be enabled." pyenv-executable))))


(provide 'obsolete/me-pyenv)
;;; me-pyenv.el ends here
