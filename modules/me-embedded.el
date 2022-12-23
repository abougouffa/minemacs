;;; me-embedded.el --- Embedded systems stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(use-package embed
  :straight (:host github :repo "xal-0/embed-el")
  :general
  (+map
    :infix "o"
    "b" '(nil :wk "embed")
    "bo" #'embed-openocd-start
    "bO" #'embed-openocd-stop
    "bg" #'embed-openocd-gdb
    "bf" #'embed-openocd-flash))

(use-package arduino-mode
  :straight (:host github :repo "bookest/arduino-mode")
  :hook (arduino-mode . display-line-numbers-mode)
  :hook (arduino-mode . hs-minor-mode))

(use-package bitbake-modes
  :straight (:host bitbucket :repo "olanilsson/bitbake-modes")
  :defer t)

(use-package vhdl-mode
  :straight (:type built-in)
  :defer t
  :config
  ;; Setup vhdl_ls from rust_hdl (AUR: rust_hdl-git)
  (+eglot-register 'vhdl-mode "vhdl_ls"))

(provide 'me-embedded)

;;; me-embedded.el ends here
