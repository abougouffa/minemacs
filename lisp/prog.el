;;; prog.el --- Programming stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <hacko@laptop>

;;; Tree sitter
(use-package tree-sitter :straight t)
(use-package tree-sitter-langs :straight t)
(use-package tree-sitter-hl)

;;; Eglot + LSP
(use-package eglot :straight t)

;;; Debug
(use-package realgud :straight t)
(use-package realgud-lldb :straight t)
(use-package realgud-ipdb :straight t)

;;; Formatting
(use-package format-all :straight t)
(use-package editorconfig :straight t)

;;; prog.el ends here
