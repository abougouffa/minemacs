;;; lisps.el --- Lisps -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <hacko@laptop>

(use-package parinfer-rust-mode
  :straight t
  :hook (emacs-lisp-mode scheme-mode common-lisp-mode))

(use-package elisp-mode
  :hook (emacs-lisp-mode . hs-minor-mode))
