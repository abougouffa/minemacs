;;; me-rainbow-delimiters.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-07-26
;; Last modified: 2025-07-26

;;; Commentary:

;;; Code:


;; Emacs rainbow delimiters mode
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode)
  :init
  (defvar +rainbow-delimiters-disabled-modes '(makefile-mode))
  :config
  (advice-add
   'rainbow-delimiters-mode :around
   (satch-defun +rainbow-delimiters--maybe:around-a (orig-fn &rest args)
     (unless (derived-mode-p +rainbow-delimiters-disabled-modes)
       (apply orig-fn args)))))


(provide 'obsolete/me-rainbow-delimiters)
;;; me-rainbow-delimiters.el ends here
