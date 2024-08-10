;;; me-alloy.el --- Alloy -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-extra-mode 'me-alloy 'alloy-mode
  :auto-mode '(("\\.als\\'" . alloy-mode)))

(use-package alloy-mode
  :straight (:host github :repo "dwwmmn/alloy-mode")
  :mode "\\.als\\'")


(provide 'modes/me-alloy)
;;; me-alloy.el ends here
