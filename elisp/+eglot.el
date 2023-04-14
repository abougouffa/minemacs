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
  (let* ((alternatives-p (length> servers 1))
         (first-server (car servers))
         (first-server (if (listp first-server) (car first-server) first-server)))
    (with-eval-after-load 'eglot
      (when (executable-find first-server)
        (add-to-list
         'eglot-server-programs
         (cons modes (if alternatives-p
                         (eglot-alternatives (ensure-list servers))
                       (ensure-list (car servers)))))))))

;;; +eglot.el ends here
