;;; me-flymake-relint.el --- Flymake integration for relint -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package flymake-relint
  :ensure t
  :hook ((emacs-lisp-mode lisp-interaction-mode) . flymake-relint-setup))


(provide 'obsolete/me-flymake-relint)
;;; me-flymake-relint.el ends here
