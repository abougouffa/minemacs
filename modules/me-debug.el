;;; me-debug.el --- Debugging stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(use-package gdb-mi
  :straight (:type built-in)
  :defer t
  :custom
  (gdb-show-main t) ; display source file containing main routine at startup
  (gdb-many-windows t) ; start in gdb-many-windows mode
  (gdb-debug-log-max 1024) ; default 128
  (gdb-restore-window-configuration-after-quit t)
  (gdb-thread-buffer-verbose-names nil)
  (gdb-window-configuration-directory (+directory-ensure (concat minemacs-local-dir "gdb/")))
  (gdb-max-source-window-count 1) ; IDEA maybe increase it!
  (gdb-display-io-nopopup nil)) ; IDEA maybe change it!

(use-package realgud
  :straight t
  :defer t
  :general
  (+map-local :keymaps '(c-mode-map c++-mode-map python-mode-map
                         sh-mode-map bash-ts-mode-map)
    "d" #'+realgud:start))

(use-package realgud-lldb
  :straight t
  :general
  (+map-local :keymaps 'rust-mode-map
    "d" #'+realgud:start)
  :commands realgud--lldb)

(use-package realgud-ipdb
  :straight t
  :defer t)

(use-package disaster
  :straight t
  :general
  (+map-local :keymaps '(c-mode-map c++-mode-map fortran-mode-map)
    "D" #'disaster))


(provide 'me-debug)
