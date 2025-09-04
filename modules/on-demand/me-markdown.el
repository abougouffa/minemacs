;;; me-markdown.el --- Markdown -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-12
;; Last modified: 2025-09-04

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-markdown
  :auto-mode '(("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))
  :companion-packages '((markdown-ts-mode . markdown-mode))
  :define-loader t)


;; Major mode for Markdown-formatted text
(use-package markdown-mode
  :straight t
  :custom
  (markdown-enable-html t)
  (markdown-enable-math t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-enable-highlighting-syntax t))


(provide 'on-demand/me-markdown)
;;; me-markdown.el ends here
