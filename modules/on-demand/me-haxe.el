;;; me-haxe.el --- Haxe langauge -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-haxe
  :auto-mode '(("\\.hx\\'" . haxe-mode)))


;; Major mode for editing Haxe files
(use-package haxe-mode
  :straight t
  :hook (haxe-mode . +prog-mode-run-hooks))


(provide 'on-demand/me-haxe)
;;; me-haxe.el ends here
