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
  :when (+emacs-options-p 'tree-sitter)
  :hook (markdown-ts-mode . display-line-numbers-mode)
  :commands (markdown-ts-mode)
  :init
  ;; Turn `markdown-ts-mode' if the buffer is big, otherwise use `markdown-mode'.
  (+alist-set "\\.md\\'" '+markdown-ts-mode-maybe auto-mode-alist)
  (defun +markdown-ts-mode-maybe (&rest _args)
    (if (< (buffer-size) 100000) (markdown-mode) (markdown-ts-mode))))


(provide 'on-demand/me-markdown)
;;; me-markdown.el ends here
