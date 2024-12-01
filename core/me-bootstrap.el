;; me-bootstrap.el --- Initialize package management -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(require 'me-lib)
(require 'package)
(require 'use-package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package--builtins '(treesit . [nil nil "Tree-Sitter integration"])) ; Some packages like `ts-movement' depends on it
(add-to-list 'package--builtins '(docker-tramp . [nil nil "Tree-Sitter integration"])) ; Needed by some packages like `ros', but provided by `tramp'

(setq
 ;; Set `use-package' to verbose when MinEmacs is started in verbose mode
 use-package-verbose (cond (minemacs-debug-p 'debug) (minemacs-verbose-p t))
 ;; Defer loading packages by default, use `:demand' to force loading a package
 use-package-always-defer (not minemacs-always-demand-p)
 use-package-always-demand minemacs-always-demand-p
 ;; Make the expanded code as minimal as possible, do not try to catch errors
 use-package-expand-minimally (not minemacs-debug-p))


;;; `use-package' extensions
;; Add `:vc' support for `use-package' in Emacs 29
(unless (> emacs-major-version 29) (require 'me-backports))

;; Prefer the newest commit over the latest release
(setq use-package-vc-prefer-newest t)

;; Add `:trigger-commands', allow loading the package before executing a
;; specific external command/function.
(add-to-list 'use-package-keywords :trigger-commands)

;; `:trigger-commands' implementation
(defun use-package-normalize/:trigger-commands (name keyword args)
  (setq args (use-package-normalize-recursive-symlist name keyword args))
  (if (consp args) args (list args)))

(defun use-package-handler/:trigger-commands (name _keyword arg rest state)
  (use-package-concat
   (unless (plist-get state :demand)
     `((satch-add-advice ',(delete-dups arg) :before (lambda (&rest _args) (require ',name)) nil :transient t)))
   (use-package-process-keywords name rest state)))


;;; Extra utilities
;; Be cautious about the installed revision of `once' and `satch' as they aren't stable yet
(use-package once
  :vc (:url "https://github.com/emacs-magus/once" :rev "a6f950c29c846a50018bc63695f24f611c1a58be"))

(use-package satch
  :vc (:url "https://github.com/emacs-magus/satch.el" :rev "77993b711cccf16702fdc8d21d8f8ba10d7bd0fb"))

;; HACK+BUG: Prevent `package' from failing when looking for depdendencies that
;; doesn't have proper versions
(advice-add
 'version-to-list :around
 (satch-defun +version-to-list--no-fail:around-a (fn ver)
   (condition-case err
       (funcall fn ver)
     (error (message "`version-to-list': %s" (error-message-string err))
            nil))))


(provide 'me-bootstrap)
;;; me-bootstrap.el ends here
