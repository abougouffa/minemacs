;;; lisp.el --- Lisp, Scheme, Elisp -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

(use-package parinfer-rust-mode
  :straight t
  :init
  (setq parinfer-rust-library-directory (expand-file-name "parinfer-rust" minemacs-var-dir)
        parinfer-rust-auto-download t
        parinfer-rust-library
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
  :straight t
  :defer t)

;; Scheme
(use-package geiser
  :straight t
  :defer t)

(use-package macrostep-geiser
  :straight t
  :defer t)

(use-package geiser-chez
  :straight t
  :defer t)

(use-package geiser-guile
  :straight t
  :defer t)

(use-package geiser-mit
  :straight t
  :defer t)

(use-package geiser-racket
  :straight t
  :defer t)

(use-package racket-mode
  :straight t
  :defer t)

(use-package elisp-mode
  :hook (emacs-lisp-mode . hs-minor-mode))

(use-package erefactor
  :straight t
  :commands (erefactor-highlight-mode
             erefactor-rename-symbol-in-buffer
             erefactor-rename-symbol-in-package))

;; (use-package elispfl
;;   :hook (emacs-lisp-mode . elispfl-mode)
;;   :straight (:type git :host github :repo "cireu/elispfl"))
