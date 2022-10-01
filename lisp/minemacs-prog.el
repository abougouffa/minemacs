;;; prog.el --- Programming stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <hacko@laptop>

;;; Tree sitter
(use-package tree-sitter
  :straight t)

(use-package tree-sitter-langs
  :straight t
  :after tree-sitter)

(use-package tree-sitter-hl
  :after tree-sitter)

;;; Eglot + LSP
(use-package eglot
  :straight t
  :commands eglot)

;;; Debug
(use-package realgud
  :straight t
  :commands (realgud:gdb
             realgud:gud
             realgud:zshdb
             realgud:bashdb
             realgud:kshdb
             realgud:pdb
             realgud:pdb-remote))

(use-package realgud-lldb
  :straight t
  :commands (realgud--lldb))

(use-package realgud-ipdb
  :straight t
  :commands (realgud:ipdb realgud:ipdb-remote))

;;; Formatting
(use-package format-all
  :straight t
  :commands (format-all-mode
             format-all-ensure-formatter
             format-all-buffer
             format-all-region))

(use-package editorconfig
  :straight t
  :commands (editorconfig-mode
             editorconfig-apply
             editorconfig-conf-mode
             editorconfig-display-current-properties
             editorconfig-find-current-editorconfig))

(use-package clang-format
  :straight t
  :commands (clang-format
             clang-format-region
             clang-format-buffer))

(provide 'minemacs-prog)

;;; prog.el ends here
