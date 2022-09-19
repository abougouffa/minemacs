;;; lisp.el --- Lisps -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <hacko@laptop>

(use-package parinfer-rust-mode
  :straight t
  :disabled t
  :init
  (setq parinfer-rust-library
        (concat doom-data-dir "parinfer-rust/"
                (cond (IS-MAC "parinfer-rust-darwin.so")
                      (IS-LINUX "parinfer-rust-linux.so")
                      (IS-WINDOWS "parinfer-rust-windows.dll")
                      (IS-BSD "libparinfer_rust.so"))))
  (setq parinfer-rust-library-directory (expand-file-name "parinfer-rust" minemacs-var-dir)
        parinfer-rust-auto-download t
        parinfer-rust-library)
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

(provide 'minemacs-lisp)
