;;; me-platformio.el --- PlateformIO -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:


;; PlatformIO integration for Emacs
(push 'projectile straight-built-in-pseudo-packages)

(use-package platformio-mode
  :straight t)

(cl-callf2 remove 'projectile straight-built-in-pseudo-packages)


(provide 'obsolete/me-platformio)
;;; me-platformio.el ends here
