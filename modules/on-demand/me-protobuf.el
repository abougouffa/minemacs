;;; me-protobuf.el --- Protobuf support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-10
;; Last modified: 2025-06-03

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-protobuf
  :auto-mode '(("\\.proto\\'" . protobuf-mode)))

(defconst +protobuf-path (concat minemacs-on-demand-modules-dir "third-party/protobuf/"))


;; Major mode for editing Protocol Buffers
(use-package protobuf-mode
  :load-path +protobuf-path
  :commands (protobuf-mode))


;; Tree-sitter based major mode for editing Protocol Buffers files
(use-package protobuf-ts-mode
  :straight (:host github :repo "emacsattic/protobuf-ts-mode")
  :when (featurep 'feat/tree-sitter))


(provide 'on-demand/me-protobuf)
;;; me-protobuf.el ends here
