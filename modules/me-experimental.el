;;; me-experimental.el --- Experimental packages -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-12-11
;; Last modified: 2026-08-07

;;; Commentary:

;;; Code:


;; Allow `package-vc' to run the build command of `lspce' (builds the Rust module)
(+package-vc-allow-build-commands 'lspce)

;; LSP Client for Emacs implemented as a module using Rust
(use-package lspce
  :vc ( :url "https://github.com/zbelial/lspce"
        ;; Build the dynamic module when `cargo' is available
        :shell-command "if command -v cargo >/dev/null 2>&1; then cargo build --release && for f in target/release/liblspce_module.so target/release/liblspce_module.dylib; do if [ -f \"$f\" ]; then cp \"$f\" \"lspce-module.${f##*.}\"; fi; done; fi")
  :when (and (featurep 'feat/modules) (not (featurep 'os/win)))
  :config
  (+defun +lspce-clangd-params () (string-join (cdr (+eglot-clangd-contact nil (project-current))) " "))
  (setcdr (assoc "C" lspce-server-programs) (list "clangd" #'+lspce-clangd-params)))


;; Alternative to `shell'/`comint' based on `term'
(use-package mistty
  :ensure t)


(provide 'me-experimental)
;;; me-experimental.el ends here
