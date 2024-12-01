;;; me-odin.el --- Odin language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-odin
  :auto-mode '(("\\.odin\\'" . odin-mode)))


;; Major mode for Odin
(use-package odin-mode
  :vc (:url "https://github.com/mattt-b/odin-mode"))


(provide 'on-demand/me-odin)
;;; me-odin.el ends here
