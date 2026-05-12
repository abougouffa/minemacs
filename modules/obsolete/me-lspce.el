;;; me-lspce.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@cneebg.pbz")
;; Created: 2026-04-01
;; Last modified: 2026-05-12

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
  (satch-defun +lspce-clangd-params () (string-join (cdr (+eglot-clangd-contact nil (project-current))) " "))
  (setcdr (assoc "C" lspce-server-programs) (list "clangd" #'+lspce-clangd-params)))


(provide 'obsolete/me-lspce)
;;; me-lspce.el ends here
