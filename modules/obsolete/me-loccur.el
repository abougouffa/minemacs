;;; me-loccur.el --- loccur integration (removed in favor of `consult-focus-lines') -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-07-27
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

(use-package loccur
  :straight t
  :bind (("C-S-o" . loccur-current)
         ("C-M-S-o" . loccur)
         ("M-s C-o" . loccur-isearch)
         :map isearch-mode-map
         ("C-o" . loccur-isearch)))


(provide 'obsolete/me-loccur)
;;; me-loccur.el ends here
