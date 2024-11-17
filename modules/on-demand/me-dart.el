;;; me-dart.el --- Dart language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-dart
  :auto-mode '(("\\.dart\\'" . dart-mode))
  :companion-packages '((dart-mode . flutter)))


;; Major mode for editing Dart files
(use-package dart-mode
  :straight t)


;; Tools for working with Flutter SDK
(use-package flutter
  :straight t)


(provide 'on-demand/me-dart)
;;; me-dart.el ends here
