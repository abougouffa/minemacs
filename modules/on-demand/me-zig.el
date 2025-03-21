;;; me-zig.el --- Zig language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-zig
  :auto-mode '(("\\.\\(zig\\|zon\\)\\'" . zig-mode)))


;; Major mode for the Zig programming language
(use-package zig-mode
  :straight t)


;; Tree-sitter based major mode for the Zig programming language
(use-package zig-ts-mode
  :straight t)


(provide 'on-demand/me-zig)
;;; me-zig.el ends here
