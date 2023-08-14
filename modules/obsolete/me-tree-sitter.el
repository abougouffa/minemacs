;;; me-tree-sitter.el --- Tree-sitter integraion for Emacs28 and earlier -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package tree-sitter
  :straight t
  :defer 5
  :when (eq sys/arch 'x86_64)
  :preface
  (+fn-inhibit-messages! tsc-dyn-get-ensure)
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :config
  (global-tree-sitter-mode 1))

(use-package tree-sitter-langs
  :straight t
  :preface
  (+fn-inhibit-messages! tree-sitter-langs-install-grammars)
  :after tree-sitter
  :demand t)


(provide 'obsolete/me-tree-sitter)

;;; me-tree-sitter.el ends here
