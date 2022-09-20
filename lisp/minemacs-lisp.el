;;; lisp.el --- Lisps -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <hacko@laptop>

(use-package parinfer-rust-mode
  :straight t
  :init
  (setq parinfer-rust-library-directory (expand-file-name "parinfer-rust" minemacs-var-dir)
        parinfer-rust-auto-download t)
  (setq parinfer-rust-library
        (expand-file-name
         (cond (MAC-P "parinfer-rust-darwin.so")
               (LINUX-P "parinfer-rust-linux.so")
               (WIN-P "parinfer-rust-windows.dll")
               (BSD-P "libparinfer_rust.so"))
         parinfer-rust-library-directory))
  :hook ((emacs-lisp-mode
          clojure-mode
          scheme-mode
          lisp-mode
          racket-mode
          hy-mode) . parinfer-rust-mode))

;; Temporary, Parinfer seems to crash!
;; (electric-pair-mode 1)

(use-package macrostep
  :defer t
  :straight t)

;; Scheme
(use-package geiser
  :defer t
  :straight t)

(use-package macrostep-geiser
  :defer t
  :straight t)

(use-package geiser-chez
  :defer t
  :straight t)

(use-package geiser-guile
  :defer t
  :straight t)

(use-package geiser-mit
  :defer t
  :straight t)

(use-package geiser-racket
  :defer t
  :straight t)

(use-package racket-mode
  :defer t
  :straight t)


(use-package elisp-mode
  :hook (emacs-lisp-mode . hs-minor-mode))

(provide 'minemacs-lisp)
