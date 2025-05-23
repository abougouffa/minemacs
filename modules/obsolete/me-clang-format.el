;;; me-clang-format.el --- Clang-format -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-12
;; Last modified: 2025-05-16

;;; Commentary:

;;; Code:

;; for bin in $(ls $(dirname $(which clang-13))/clang-*); do ln -s $bin $HOME/.local/bin/$(basename ${bin%-13}); done
(use-package clang-format
  :straight t
  :config
  (advice-add
   'clang-format-region :before
   (satch-defun +clang-format--set-style ()
     "Automatically set the `clang-format' style if not already set."
     (unless (local-variable-p 'clang-format-style)
       (setq-local clang-format-style (+clang-format-get-style t))))))


(provide 'obsolete/me-clang-format)
;;; me-clang-format.el ends here
