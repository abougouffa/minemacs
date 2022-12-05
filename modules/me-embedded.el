;;; me-embedded.el --- Embedded systems stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

(use-package embed
  :straight (:type git :host github :repo "xal-0/embed-el")
  :general
  (+map
    "ob" '(nil :wk "embed")
    "obo" #'embed-openocd-start
    "obO" #'embed-openocd-stop
    "obg" #'embed-openocd-gdb
    "obf" #'embed-openocd-flash))


(use-package arduino-mode
  :straight (:type git :host github :repo "bookest/arduino-mode")
  :defer t
  :hook (arduino-mode . display-line-numbers-mode)
  :hook (arduino-mode . hs-minor-mode))


(use-package bitbake-modes
  :straight (:type git :host bitbucket :repo "olanilsson/bitbake-modes")
  :defer t)


(provide 'me-embedded)

;;; me-embedded.el ends here
