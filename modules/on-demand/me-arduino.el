;;; me-arduino.el --- Arduino support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-10
;; Last modified: 2025-12-20

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-arduino
  :auto-mode '((("\\.ino\\'" "\\.pde\\'") . +arduino-mode)))

(if (featurep 'feat/tree-sitter)
    (define-derived-mode +arduino-mode c++-ts-mode "Arduino" "Alias for `c++-ts-mode' to edit Arduino files.")
  (define-derived-mode +arduino-mode c++-mode "Arduino" "Alias for `c++-mode' to edit Arduino files."))

(add-hook 'auto-mode-alist '("\\.\\(ino\\|pde\\)\\'" . +arduino-mode))


;; Arduino CLI command wrapper
(use-package arduino-cli-mode
  :straight t
  :hook +arduino-mode
  :custom
  (arduino-cli-verify t))


(provide 'on-demand/me-arduino)
;;; me-arduino.el ends here
