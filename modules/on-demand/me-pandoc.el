;;; me-pandoc.el --- Pandoc extension -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-pandoc
  :companion-packages '(((markdown-mode markdown-ts-mode org-mode tex-mode latex-mode) . pandoc-mode)))


;; Minor mode for interacting with Pandoc
(use-package pandoc-mode
  :straight t
  :hook ((markdown-mode markdown-ts-mode org-mode tex-mode latex-mode) . conditionally-turn-on-pandoc))


(provide 'on-demand/me-pandoc)
;;; me-pandoc.el ends here
