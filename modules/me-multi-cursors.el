;;; me-multi-cursors.el --- Programming stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package iedit
  :straight t
  :bind ("C-;" . iedit-mode))

(use-package multiple-cursors
  :straight t
  :config
  (cl-callf append mc--default-cmds-to-run-for-all
    '(meow-insert meow-append meow-backward-delete
      meow-delete meow-replace meow-replace-char
      meow-yank meow-yank-pop meow-kill)))


(provide 'me-multi-cursors)

;;; me-multi-cursors.el ends here
