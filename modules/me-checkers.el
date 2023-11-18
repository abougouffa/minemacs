;;; me-checkers.el --- Syntax checking -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package flymake-easy
  :straight t
  :autoload flymake-easy-load)

(use-package flymake-ruff
  :straight t
  :hook ((python-mode python-ts-mode) . flymake-ruff-load))

(use-package flymake-collection
  :straight t
  :hook (minemacs-after-startup . flymake-collection-hook-setup))

(use-package flymake-shellcheck
  :straight t
  :commands flymake-shellcheck-load
  :hook ((sh-mode bash-ts-mode) . flymake-shellcheck-load))


(provide 'me-checkers)

;;; me-checkers.el ends here
