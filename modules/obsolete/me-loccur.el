;;; me-loccur.el --- loccur integration (removed in favor of `consult-focus-lines') -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

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
