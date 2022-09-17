;;; prog.el --- Programming stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <hacko@laptop>

;;; Tree sitter
(use-package tree-sitter
	:straight t
	:defer t)

(use-package tree-sitter-langs
	:straight t
	:after tree-sitter)

(use-package tree-sitter-hl
	:after tree-sitter)

;;; Eglot + LSP
(use-package eglot
	:straight t
	:commands (eglot))

;;; Debug
(use-package realgud
	:straight t
	:commands (realgud:gdb))

(use-package realgud-lldb
	:straight t
	:commands (realgud--lldb))

(use-package realgud-ipdb
	:straight t
	:commands (realgud:ipdb realgud:ipdb-remote))

;;; Formatting
(use-package format-all
	:straight t
	:commands (format-all-ensure-formatter
						 format-all-mode
						 format-all-buffer
						 format-all-region))

(use-package editorconfig
	:straight t
	:commands (editorconfig-display-current-properties
						 editorconfig-conf-mode
						 editorconfig-find-current-editorconfig
						 editorconfig-apply
						 editorconfig-mode))

;;; prog.el ends here
