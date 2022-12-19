;;; lisp.el --- Lisp, Scheme, Elisp -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(use-package parinfer-rust-mode
  :straight t
  :when (+emacs-features-p 'modules)
  :custom
  (parinfer-rust-library-directory (+expand 'local "parinfer-rust" t))
  (parinfer-rust-auto-download t)
  :hook ((emacs-lisp-mode
          clojure-mode
          scheme-mode
          lisp-mode
          racket-mode
          hy-mode) . parinfer-rust-mode))

(use-package slime
  :straight t
  :defer t
  :init
  (setq inferior-lisp-program "sbcl"))

(use-package macrostep
  :straight t
  :defer t
  :general
  (+map-local :keymaps 'emacs-lisp-mode-map
    "m" '(macrostep-expand :wk "Expand macro")))

(use-package macrostep-geiser
  :straight t
  :after geiser
  :general
  (+map-local :keymaps '(scheme-mode-map racket-mode-map)
    "m" '(macrostep-geiser-expand-all :wk "Expand macro"))
  :config
  (macrostep-geiser-setup))

;; Scheme
(use-package geiser
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

;; TODO: Add elisp-def
(use-package elisp-mode
  :hook (emacs-lisp-mode . (lambda () (setq-local tab-width 8))) ;; to better view built-in packages
  :after minemacs-loaded ;; prevent elisp-mode from being loaded too early
  :config
  (+map-local :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "d"   '(nil :wk "edebug")
    "df"  'edebug-defun
    "dF"  'edebug-all-forms
    "dd"  'edebug-all-defs
    "dr"  'edebug-remove-instrumentation
    "do"  'edebug-on-entry
    "dO"  'edebug-cancel-on-entry
    "db"  '(nil :wk "breakpoints")
    "dbb" 'edebug-set-breakpoint
    "dbr" 'edebug-unset-breakpoint
    "dbn" 'edebug-next-breakpoint
    "e"   '(nil :wk "eval")
    "eb"  'eval-buffer
    "ed"  'eval-defun
    "ee"  'eval-last-sexp
    "er"  'eval-region
    "el"  'load-library
    "g"   '(nil :wk "goto/find")
    "gf"  'find-function-at-point
    "gR"  'find-function
    "gv"  'find-variable-at-point
    "gV"  'find-variable
    "gL"  'find-library)

  (+map-local :keymaps '(edebug-mode-map)
    "e"   '(nil :wk "eval")
    "ee"  'edebug-eval-last-sexp
    "eE"  'edebug-eval-expression
    "et"  'edebug-eval-top-level-form))

(use-package me-elisp-extras
  :after elisp-mode minemacs-loaded
  :config
  (+elisp-indent-setup)
  (+elisp-highlighting-setup))

(use-package elisp-demos
  :straight t
  :after elisp-mode minemacs-loaded
  :init
  (advice-add #'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add #'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package helpful
  :straight t
  :after elisp-mode minemacs-loaded
  :commands helpful-symbol helpful-command helpfull-callable helpful-at-point
  :general
  (+map
    "hp" #'helpful-at-point
    "ho" #'helpful-symbol
    "hc" #'helpful-command
    "hF" #'helpful-function
    "hf" #'helpful-callable))

(use-package eros
  :straight t
  :after elisp-mode minemacs-loaded
  :custom
  (eros-eval-result-prefix "⟹ ")
  :config
  (eros-mode 1))


(provide 'me-lisp)
