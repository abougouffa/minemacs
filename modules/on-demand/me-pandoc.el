;;; me-pandoc.el --- Pandoc extension -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-pandoc
  :companion-packages '(((markdown-mode markdown-ts-mode org-mode tex-mode latex-mode) . pandoc-mode)))

(use-package pandoc-mode
  :straight t
  :hook ((markdown-mode markdown-ts-mode org-mode tex-mode latex-mode) . conditionally-turn-on-pandoc))


(provide 'on-demand/me-pandoc)
;;; me-pandoc.el ends here
