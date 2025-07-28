;;; me-protobuf.el --- Protobuf support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-10
;; Last modified: 2025-07-28

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-protobuf
  :auto-mode '(("\\.proto\\'" . protobuf-mode)))

(defconst +protobuf-path (concat minemacs-on-demand-modules-dir "third-party/protobuf/"))


;; Major mode for editing Protocol Buffers
(use-package protobuf-mode
  :load-path +protobuf-path
  :mode "\\.proto\\'"
  :commands (protobuf-mode))


;; Tree-sitter based major mode for editing Protocol Buffers files
(use-package protobuf-ts-mode
  :straight (:host github :repo "emacsattic/protobuf-ts-mode")
  :when (featurep 'feat/tree-sitter)
  :config
  (add-to-list 'treesit-language-source-alist '(proto "https://github.com/mitchellh/tree-sitter-proto"))
  (treesit-ensure-installed 'proto))


(provide 'on-demand/me-protobuf)
;;; me-protobuf.el ends here
