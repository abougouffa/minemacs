;;; lisps.el --- Lisps -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <hacko@laptop>

(use-package parinfer-rust-mode
  :straight t
  :disabled t
	:hook ((emacs-lisp-mode
          clojure-mode
          scheme-mode
          lisp-mode
          racket-mode
          hy-mode) . parinfer-rust-mode))

;; Temporary, Parinfer seems to crash!
(electric-pair-mode 1)

(use-package elisp-mode
  :hook (emacs-lisp-mode . hs-minor-mode))
