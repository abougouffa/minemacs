;;; me-experimental.el --- Experimental packages -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-12-11
;; Last modified: 2025-07-25

;;; Commentary:

;;; Code:


;; A collection of commands, modes and functions built on top of the keyboard macros (kmacros)
(use-package kmacro-x
  :straight t
  :hook (minemacs-first-file . kmacro-x-atomic-undo-mode)
  ;; :bind (("C-<" . kmacro-x-mc-mark-previous)
  ;;        ("C->" . kmacro-x-mc-mark-next))
  :custom
  (kmacro-x-mc-live-preview t))


(provide 'me-experimental)
;;; me-experimental.el ends here
