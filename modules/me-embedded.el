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


(provide 'me-prog)

;;; me-embedded.el ends here
