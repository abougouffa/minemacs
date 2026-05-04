;;; me-lexic.el --- StarDict integration -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2026-05-04
;; Last modified: 2026-05-04

;;; Commentary:

;;; Code:


;; Fancy Emacs integration with the console version of StarDict
(use-package lexic
  :straight t
  :when (executable-find "sdcv"))


(provide 'obsolete/me-lexic)
;;; me-lexic.el ends here
