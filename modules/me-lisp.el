;;; lisp.el --- Lisp, Scheme, Elisp -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(when (+emacs-features-p 'modules)
  (use-package parinfer-rust-mode
    :straight t
    :when (eq sys/arch 'x86_64)
    :custom
    (parinfer-rust-library-directory (concat minemacs-local-dir "parinfer-rust/"))
    (parinfer-rust-auto-download (eq sys/arch 'x86_64))
    :hook ((emacs-lisp-mode
            clojure-mode
            scheme-mode
            lisp-mode
            racket-mode
            hy-mode)
           . parinfer-rust-mode)))

;; Common Lisp
(use-package sly
  :straight t
  :custom
  (sly-mrepl-history-file-name (+directory-ensure minemacs-local-dir "sly/mrepl-history.el"))
  (sly-net-coding-system 'utf-8-unix)
  :config
  (dolist (impl '("lisp"   ; Default Lisp implementation on the system
                  "clisp"  ; GNU CLISP
                  "abcl"   ; Armed Bear Common Lisp
                  "ecl"    ; Embeddable Common-Lisp
                  "gcl"    ; GNU Common Lisp
                  "ccl"    ; Clozure Common Lisp
                  "cmucl"  ; CMU Common Lisp
                  "clasp"  ; Common Lisp on LLVM
                  "sbcl")) ; Steel Bank Common Lisp
    (when (executable-find impl)
      (add-to-list
       'sly-lisp-implementations
       `(,(intern impl) (,impl) :coding-system utf-8-unix))))
  (setq inferior-lisp-program (caar (cdar sly-lisp-implementations))
        sly-default-lisp (caar sly-lisp-implementations))

  (+map-local! :keymaps '(lisp-mode-map)
    "s"  #'sly
    "c"  '(nil :wk "compile")
    "cc" #'sly-compile-file
    "cC" #'sly-compile-and-load-file
    "cd" #'sly-compile-defun
    "cr" #'sly-compile-region
    "g"  '(nil :wk "goto/find")
    "gn" #'sly-goto-first-note
    "gL" #'sly-load-file
    "gn" #'sly-next-note
    "gN" #'sly-previous-note
    "gs" #'sly-stickers-next-sticker
    "gS" #'sly-stickers-prev-sticker
    "gN" #'sly-previous-note
    "gd" #'sly-edit-definition
    "gD" #'sly-edit-definition-other-window
    "gb" #'sly-pop-find-definition-stack
    "h"  '(nil :wk "help/info")
    "hs" #'sly-describe-symbol
    "hf" #'sly-describe-function
    "hc" #'sly-who-calls
    "hC" #'sly-calls-who
    "hs" #'sly-who-calls
    "hC" #'sly-calls-who
    "hd" #'sly-disassemble-symbol
    "hD" #'sly-disassemble-definition
    "r"  '(nil :wk "repl")
    "rr" #'sly-restart-inferior-lisp
    "rc" #'sly-mrepl-clear-repl
    "rs" #'sly-mrepl-sync
    "rn" #'sly-mrepl-new
    "rq" #'sly-quit-lisp))

;; Scheme
(use-package racket-mode
  :straight t)

(use-package geiser
  :straight t
  :custom
  (geiser-default-implementation 'guile))

(use-package geiser-chez
  :straight t)

(use-package geiser-chibi
  :straight t)

(use-package geiser-chicken
  :straight t)

(use-package geiser-gambit
  :straight t)

(use-package geiser-gauche
  :straight t)

(use-package geiser-guile
  :straight t)

(use-package geiser-kawa
  :straight t)

(use-package geiser-mit
  :straight t)

(use-package geiser-racket
  :straight t)

(use-package geiser-stklos
  :straight t)

;; Clojure
(use-package clojure-mode
  :straight t)

(use-package cider
  :straight t)

;; Macro expansion
(use-package macrostep
  :straight (macrostep :fork (:host github :repo "abougouffa/macrostep" :branch "fix_keymap"))
  :init
  (+map-local! :keymaps '(emacs-lisp-mode-map lisp-mode-map)
    "m" '(macrostep-expand :wk "Expand macro")))

(use-package macrostep-geiser
  :straight t
  :after geiser
  :hook ((geiser-mode geiser-repl-mode) . macrostep-geiser-setup)
  :init
  (+map-local! :keymaps '(geiser-mode-map geiser-repl-mode-map)
    "m" '(macrostep-expand :wk "Expand macro")
    "M" #'macrostep-geiser-expand-all))

(use-package sly-macrostep
  :straight t
  :after sly
  :demand t
  :init
  (+map-local! :keymaps '(sly-mode-map sly-editing-mode-map sly-mrepl-mode-map)
    "m" '(macrostep-expand :wk "Expand macro")))

;; Emacs Lisp
(use-package elisp-mode
  :straight (:type built-in)
  :hook (emacs-lisp-mode . (lambda () (setq-local tab-width 8))) ;; to view built-in packages correctly
  :after minemacs-loaded ; prevent elisp-mode from being loaded too early
  :init
  (+map-local! :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map ielm-map lisp-mode-map racket-mode-map scheme-mode-map)
    "p" #'check-parens)
  :config
  (+map-local! :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
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
  (+map-local! :keymaps '(edebug-mode-map)
    "e"   '(nil :wk "eval")
    "ee"  'edebug-eval-last-sexp
    "eE"  'edebug-eval-expression
    "et"  'edebug-eval-top-level-form))

(use-package me-elisp-extras
  :after elisp-mode minemacs-loaded
  :demand t
  :config
  (+elisp-indent-setup)
  (+elisp-highlighting-setup))

(use-package elisp-demos
  :straight t
  :after elisp-mode minemacs-loaded
  :demand t
  :init
  (+map! :infix "he"
    "d" #'elisp-demos-find-demo
    "D" #'elisp-demos-add-demo)
  (advice-add #'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add #'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package helpful
  :straight t
  :init
  (+map! :keymaps 'emacs-lisp-mode-map
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
  :demand t
  :custom
  (eros-eval-result-prefix "⟹ ")
  :config
  (eros-mode 1))


(provide 'me-lisp)

;;; me-lisp.el ends here
