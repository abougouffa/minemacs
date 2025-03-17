;;; me-markdown.el --- Markdown -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-markdown
  :auto-mode '(("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))
  :companion-packages '((markdown-mode . markdown-ts-mode)))


;; Major mode for Markdown-formatted text
(use-package markdown-mode
  :straight t
  :custom
  (markdown-enable-html t)
  (markdown-enable-math t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-enable-highlighting-syntax t))


;; Major mode for Markdown using Treesitter
(use-package markdown-ts-mode
  :straight t
  :when (and (+emacs-options-p 'tree-sitter)
             (< emacs-major-version 31)) ;; Built-in in Emacs 31+
  :hook (markdown-ts-mode . display-line-numbers-mode)
  :commands (markdown-ts-mode))


(provide 'on-demand/me-markdown)
;;; me-markdown.el ends here
