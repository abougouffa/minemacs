;;; lisp.el --- Lisp, Scheme, Elisp -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(use-package parinfer-rust-mode
  :straight t
  :when (+emacs-features-p 'modules)
  :custom
  (parinfer-rust-library-directory (concat minemacs-local-dir "parinfer-rust/"))
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
  :config
  (dolist (impl '("lisp" "clisp" "ccl" "cmucl" "sbcl"))
    (when (executable-find impl)
      (add-to-list
       'slime-lisp-implementations
       `(,(intern impl) (,impl) :coding-system utf-8-unix))))
  (setq inferior-lisp-program (caar (cdar slime-lisp-implementations))
        slime-default-lisp (caar slime-lisp-implementations)))

(use-package macrostep
  :straight t
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
  :defer t
  :custom
  (geiser-default-implementation 'guile))

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
  :straight (:type built-in)
  :hook (emacs-lisp-mode . (lambda () (setq-local tab-width 8))) ;; to view built-in packages correctly
  :after minemacs-loaded ;; prevent elisp-mode from being loaded too early
  :general
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
    "eR"  'elisp-eval-region-or-buffer
    "el"  'load-library
    "g"   '(nil :wk "goto/find")
    "gf"  'find-function-at-point
    "gR"  'find-function
    "gv"  'find-variable-at-point
    "gV"  'find-variable
    "gL"  'find-library
    "c"   '(nil :wk "compile")
    "cc"  #'elisp-byte-compile-buffer
    "cf"  #'elisp-byte-compile-file
    "cn"  #'emacs-lisp-native-compile-and-load
    "cb"  #'emacs-lisp-byte-compile-and-load)
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
  (advice-add #'helpful-update :after #'elisp-demos-advice-helpful-update)
  :general
  (+map
    :infix "he"
    "d" #'elisp-demos-find-demo
    "D" #'elisp-demos-add-demo))

(use-package helpful
  :straight t
  :general
  (+map :keymaps 'emacs-lisp-mode-map
    :infix "h"
    "p" #'helpful-at-point
    "o" #'helpful-symbol
    "c" #'helpful-command
    "F" #'helpful-function
    "f" #'helpful-callable))

(use-package info-colors
  :straight t
  :hook (Info-selection . info-colors-fontify-node))

(use-package eros
  :straight t
  :after elisp-mode minemacs-loaded
  :custom
  (eros-eval-result-prefix "⟹ ")
  :config
  (eros-mode 1))


(provide 'me-lisp)
