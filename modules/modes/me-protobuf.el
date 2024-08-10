;;; me-protobuf.el --- Protobuf support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-extra-mode 'me-protobuf '(protobuf-mode protobuf-ts-mode)
  :auto-mode '(("\\.proto\\'" . protobuf-mode)))

(use-package protobuf-mode
  :straight t)

(use-package protobuf-ts-mode
  :straight (:host github :repo "emacsattic/protobuf-ts-mode")
  :when (+emacs-features-p 'tree-sitter))


(provide 'modes/me-protobuf)
;;; me-protobuf.el ends here
