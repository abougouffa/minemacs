;;; me-arduino.el --- Arduino support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-arduino
  :auto-mode '((("\\.ino\\'" "\\.pde\\'") . arduino-mode)))


;; Major mode for the Arduino language
(use-package arduino-mode
  :straight (:host github :repo "bookest/arduino-mode")
  :hook (arduino-mode . +prog-mode-run-hooks))


(provide 'on-demand/me-arduino)
;;; me-arduino.el ends here
