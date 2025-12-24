;;; me-pyenv.el --- Python environment integration (replaced with `pet') -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-05-16
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

(use-package pyvenv
  :ensure t)

;; Emacs integration for "pyenv"
(use-package pyenv
  :vc (:url "https://github.com/aiguofer/pyenv.el")
  :hook (minemacs-first-file . global-pyenv-mode)
  :custom
  (pyenv-show-active-python-in-modeline nil)
  :init
  (when-let* ((exe (executable-find "pyenv")))
    (setq pyenv-executable exe)) ; In some cases, pyenv is installed under "/usr/bin/pyenv"
  (advice-add 'pyenv-use :around #'+apply-suppress-messages))


(provide 'obsolete/me-pyenv)
;;; me-pyenv.el ends here
