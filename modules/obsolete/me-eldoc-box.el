;;; me-eldoc-box.el --- Eldoc in a childframe -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-06-16
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

(use-package eldoc-box
  :straight t
  :hook (prog-mode . +eldoc-box-hover-at-point-mode-maybe)
  :hook (eglot-managed-mode . +eldoc-box-hover-at-point-mode-maybe)
  :init
  (defun +eldoc-box-hover-at-point-mode-maybe (&optional arg)
    (when (display-graphic-p)
      (eldoc-box-hover-at-point-mode arg))))


(provide 'obsolete/me-eldoc-box)
;;; me-eldoc-box.el ends here
