;;; me-forth.el --- Forth language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-forth
  :auto-mode '(("\\.\\(fth\\|4th\\)\\'" . forth-mode)))


;; Major mode for the Forth programming language
(use-package forth-mode
  :straight t)


(provide 'on-demand/me-forth)
;;; me-forth.el ends here
