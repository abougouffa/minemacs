;;; me-fish.el --- Fish shell -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-fish
  :auto-mode '((("\\.fish\\'" "/fish_funced\\..*\\'") . fish-mode))
  :interpreter-mode '(("fish" . fish-mode)))


;; Major mode for Fish shell scripts
(use-package fish-mode
  :straight t)


(provide 'on-demand/me-fish)
;;; me-fish.el ends here
