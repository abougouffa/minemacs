;;; me-doom-modeline.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-07-18
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:


;; A fancy and fast mode-line inspired by minimalism design
(use-package doom-modeline
  :ensure t
  :hook (minemacs-lazy . doom-modeline-mode)
  :custom
  (doom-modeline-height 24)
  (doom-modeline-time-icon nil)
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-total-line-number t)
  (doom-modeline-enable-word-count t)
  (doom-modeline-continuous-word-count-modes '(markdown-mode markdown-ts-mode gfm-mode org-mode rst-mode latex-mode tex-mode)))


(provide 'obsolete/me-doom-modeline)
;;; me-doom-modeline.el ends here
