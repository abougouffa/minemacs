;;; me-common-lisp.el --- Common Lisp packages       -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

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

(use-package sly-quicklisp
  :straight t
  :after sly
  :demand t)

(use-package sly-asdf
  :straight t
  :after sly
  :demand t)

(use-package sly-repl-ansi-color
  :straight t
  :after sly
  :demand t)

(use-package sly-macrostep
  :straight t
  :after sly
  :demand t
  :init
  (+map-local! :keymaps '(sly-mode-map sly-editing-mode-map sly-mrepl-mode-map)
    "m" '(macrostep-expand :wk "Expand macro")))


(provide 'me-common-lisp)
;;; me-common-lisp.el ends here
