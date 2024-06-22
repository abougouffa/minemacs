;;; me-keybindings.el --- Default keybindings -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package hydra
  :straight t)

;; TODO: https://karthinks.com/software/avy-can-do-anything
;; TODO: Maybe move to a new `me-navigation' module
(use-package avy
  :straight t
  :bind (("C-\"" . avy-goto-char)
         ("C-é" . avy-goto-line) ; French AZERTY
         ("M-g l" . avy-goto-line)
         ("M-j" . avy-goto-char-timer))) ; Instead of `default-indent-new-line'

(use-package key-chord
  :straight t)


(provide 'me-keybindings)

;;; me-keybindings.el ends here
