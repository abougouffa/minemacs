;;; me-lspce.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@cneebg.pbz")
;; Created: 2026-04-01
;; Last modified: 2026-04-01

;;; Commentary:

;;; Code:


;; LSP Client for Emacs implemented as a module using Rust
(use-package lspce
  :straight `( lspce :host github :repo "zbelial/lspce"
               :files (:defaults ,(file-name-with-extension "lspce-module" module-file-suffix))
               :pre-build ,(when (executable-find "cargo")
                             `(("cargo" "build" "--release")
                               ("cp"
                                ,(file-name-with-extension "./target/release/liblspce_module" module-file-suffix)
                                ,(file-name-with-extension "./lspce-module" module-file-suffix)))))
  :when (and (featurep 'feat/modules) (not (featurep 'os/win)))
  :config
  (add-to-list
   'lspce-server-programs
   '("C/C++" "clangd" "--background-index" "-j=12" "--clang-tidy"
     "--all-scopes-completion" "--cross-file-rename" "--completion-style=detailed"
     "--header-insertion-decorators" "--header-insertion=iwyu" "--pch-storage=memory")))


(provide 'obsolete/me-lspce)
;;; me-lspce.el ends here
