;;; me-dart.el --- Dart language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn-rkg@fntrzpbz.pbz")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-dart
  :auto-mode '(("\\.dart\\'" . dart-mode))
  :companion-packages '((dart-mode . flutter)))

(use-package dart-mode
  :straight t)

(use-package flutter
  :straight t)


(provide 'on-demand/me-dart)
;;; me-dart.el ends here
