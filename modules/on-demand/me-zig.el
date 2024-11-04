;;; me-zig.el --- Zig language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-zig
  :auto-mode '(("\\.\\(zig\\|zon\\)\\'" . zig-mode)))

(use-package zig-mode
  :straight t)

(use-package zig-ts-mode
  :straight t)


(provide 'on-demand/me-zig)
;;; me-zig.el ends here
