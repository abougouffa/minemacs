;;; me-dart.el --- Dart language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-12
;; Last modified: 2025-08-31

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-dart
  :auto-mode '(("\\.dart\\'" . dart-mode))
  :companion-packages '((dart-mode . flutter)))


;; Major mode for editing Dart files
(use-package dart-mode
  :straight t)


;; A major mode for Dart programming language with tree-sitter supports
(use-package dart-ts-mode
  :straight (:host github :repo "50ways2sayhard/dart-ts-mode")
  :config
  (add-to-list
   'treesit-language-source-alist
   '(dart "https://github.com/UserNobody14/tree-sitter-dart"))
  (treesit-ensure-installed 'dart))


;; Tools for working with Flutter SDK
(use-package flutter
  :straight t)


(provide 'on-demand/me-dart)
;;; me-dart.el ends here
