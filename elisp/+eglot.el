;; -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

;;;###autoload
(defun +eglot-register (modes &rest servers)
  "Register MODES with LSP SERVERS.
Examples:
  (+eglot-register 'vhdl-mode \"vhdl_ls\")
  (+eglot-register 'lua-mode \"lua-language-server\" \"lua-lsp\")
  (+eglot-register '(c-mode c++-mode) '(\"clangd\" \"--clang-tidy\" \"-j=12\") \"ccls\")"
  (declare (indent 0))
  (with-eval-after-load 'eglot
    (add-to-list
     'eglot-server-programs
     (cons modes (if (length> servers 1)
                     (eglot-alternatives (ensure-list servers))
                   (ensure-list (car servers)))))))

;;; +eglot.el ends here
