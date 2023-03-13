;;; me-embedded.el --- Embedded systems stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")


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
  :straight (:host bitbucket :repo "olanilsson/bitbake-modes"))

(use-package vhdl-mode
  :straight (:type built-in)
  :config
  ;; Setup vhdl_ls from rust_hdl (AUR: rust_hdl-git)
  (+eglot-register 'vhdl-mode "vhdl_ls"))

(use-package verilog-mode
  :straight (:type built-in)
  :config
  ;; Setup Verilog/SystemVerilog LSP servers
  (+eglot-register 'verilog-mode "svls" "verible-verilog-ls" "svlangserver"))

(use-package mips-mode
  :straight t
  :mode "\\.mips\\'")

(use-package riscv-mode
  :straight t
  :mode "\\.riscv\\'")

(use-package x86-lookup
  :straight t
  :custom
  (x86-lookup-browse-pdf-function 'x86-lookup-browse-pdf-pdf-tools)
  ;; Get manual from intel.com/content/www/us/en/developer/articles/technical/intel-sdm.html
  (x86-lookup-pdf (concat minemacs-local-dir "x86-lookup/325383-sdm-vol-2abcd.pdf")))


(provide 'me-embedded)

;;; me-embedded.el ends here
