;;; me-forth.el --- Forth language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-12
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-forth
  :auto-mode '(("\\.\\(fth\\|4th\\)\\'" . forth-mode)))


;; Major mode for the Forth programming language
(use-package forth-mode
  :ensure t)


(provide 'on-demand/me-forth)
;;; me-forth.el ends here
