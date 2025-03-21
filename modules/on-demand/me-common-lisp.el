;;; me-common-lisp.el --- Common Lisp support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-common-lisp
  :companion-packages '((lisp-mode . (sly sly-macrostep sly-quicklisp sly-asdf sly-macrostep))))


;; Sylvester the Cat's Common Lisp IDE
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
        sly-default-lisp (caar sly-lisp-implementations)))


;; Quicklisp support for SLY
(use-package sly-quicklisp
  :straight t
  :after sly
  :demand)


;; ASDF system support for SLY
(use-package sly-asdf
  :straight t
  :after sly
  :demand)


;; Add ANSI colors support to the `sly-mrepl'
(use-package sly-repl-ansi-color
  :straight t
  :after sly
  :demand)


;; Fancy macro-expansion via `macrostep'
(use-package sly-macrostep
  :straight t
  :after sly
  :demand)


(provide 'on-demand/me-common-lisp)
;;; me-common-lisp.el ends here
