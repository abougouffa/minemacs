;;; lisp.el --- Lisp, Scheme, Elisp -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

(use-package parinfer-rust-mode
  :straight t
  :when feat/modules
  :init
  (setq parinfer-rust-library-directory (expand-file-name "parinfer-rust" minemacs-var-dir)
        parinfer-rust-auto-download t
        parinfer-rust-library (expand-file-name
                               (cond (sys/linux "parinfer-rust-linux.so")
                                     (sys/bsd "libparinfer_rust.so")
                                     (sys/win "parinfer-rust-windows.dll")
                                     (sys/mac "parinfer-rust-darwin.so"))
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
  :defer t
  :general
  (me-map-local :keymaps 'emacs-lisp-mode-map
    "m" '(macrostep-expand :which-key "Expand macro")))


(use-package macrostep-geiser
  :straight t
  :after geiser
  :general
  (me-map-local :keymaps '(scheme-mode-map racket-mode-map)
    "m" '(macrostep-geiser-expand-all :which-key "Expand macro"))
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
  :hook (emacs-lisp-mode . hs-minor-mode)
  :after minemacs-loaded ;; prevent elisp-mode from being loaded too early
  :config
  (me-log! "Loaded elisp-mode")
  (require 'me-elisp-extras)

  (me-map-local :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "d"   '(nil :which-key "edebug")
    "df"  'edebug-defun
    "dF"  'edebug-all-forms
    "dd"  'edebug-all-defs
    "dr"  'edebug-remove-instrumentation
    "do"  'edebug-on-entry
    "dO"  'edebug-cancel-on-entry
    "db"  '(nil :which-key "breakpoints")
    "dbb" 'edebug-set-breakpoint
    "dbr" 'edebug-unset-breakpoint
    "dbn" 'edebug-next-breakpoint
    "e"   '(nil :which-key "eval")
    "eb"  'eval-buffer
    "ed"  'eval-defun
    "ee"  'eval-last-sexp
    "er"  'eval-region
    "el"  'load-library
    "g"   '(nil :which-key "goto/find")
    "gf"  'find-function-at-point
    "gR"  'find-function
    "gv"  'find-variable-at-point
    "gV"  'find-variable
    "gL"  'find-library)

  (me-map-local :keymaps '(edebug-mode-map)
    "e"   '(nil :which-key "eval")
    "ee"  'edebug-eval-last-sexp
    "eE"  'edebug-eval-expression
    "et"  'edebug-eval-top-level-form)

  (me-elisp-indent-setup)
  (me-elisp-highlighting-setup))


(use-package erefactor
  :straight t
  :hook ((emacs-lisp-mode lisp-interaction-mode) . erefactor-lazy-highlight-turn-on)
  :general
  (me-map-local :keymaps '(emacs-lisp-mode-map lisp-data-mode-map)
    "r"  '(nil :which-key "refactor")
    "rr" '(erefactor-rename-symbol-in-buffer :which-key "Rename symbol in buffer")
    "rR" '(erefactor-rename-symbol-in-package :which-key "Rename symbol in package")))


(use-package elisp-demos
  :straight t
  :after elisp-mode minemacs-loaded
  :init
  (advice-add #'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add #'helpful-update :after #'elisp-demos-advice-helpful-update))


(use-package helpful
  :straight t
  :commands (helpful-symbol helpful-command helpfull-callable helpful-at-point))


(provide 'me-lisp)
