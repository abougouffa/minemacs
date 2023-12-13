;;; me-embedded.el --- Embedded systems stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package embed
  :straight (:host github :repo "xal-0/embed-el")
  :init
  (+map! :infix "o"
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
  :config
  (+map-local! :keymaps 'bitbake-mode-map
    "i" '(nil "insert/edit")
    "ii" #'bitbake-inc-pr
    "b"  #'bitbake-recipe-build-dir-dired))

(use-package bitbake-electric
  :hook (bitbake-mode . bitbake-electric-mode))

(use-package bitbake-insert
  :after bitbake
  :demand t
  :config
  (+map-local! :keymaps 'bitbake-mode-map
    "i"  '(nil "insert/edit")
    "iv" #'bitbake-insert-var
    "ia" #'bitbake-append-var
    "io" #'bitbake-insert-override))

(use-package mips-mode
  :straight t)

(use-package riscv-mode
  :straight t)

(use-package x86-lookup
  :straight t
  :custom
  (x86-lookup-browse-pdf-function 'x86-lookup-browse-pdf-pdf-tools)
  ;; Get manual from intel.com/content/www/us/en/developer/articles/technical/intel-sdm.html
  (x86-lookup-pdf (concat minemacs-local-dir "intel-64-and-ia32-volumes-1234.pdf"))
  :config
  (unless (file-exists-p x86-lookup-pdf)
    (url-copy-file "https://cdrdv2.intel.com/v1/dl/getContent/671200" x86-lookup-pdf t)))


(provide 'me-embedded)

;;; me-embedded.el ends here
