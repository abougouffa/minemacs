;;; me-freebasic.el --- FreeBasic support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-extra-mode 'me-freebasic 'fb-mode
  :auto-mode '(("\\.b\\(i\\|as\\)\\'" . fb-mode)))

(use-package fb-mode
  :straight (:host github :repo "rversteegen/fb-mode")
  :commands (fb-mode)
  :mode "\\.b\\(i\\|as\\)\\'")


(provide 'modes/me-freebasic)
;;; me-freebasic.el ends here
