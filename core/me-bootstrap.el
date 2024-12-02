;; me-bootstrap.el --- Initialize package management -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(require 'me-lib)
(require 'package)
(require 'use-package)
(require 'package-vc nil :noerror)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq package-user-dir (expand-file-name "elpa" minemacs-local-dir)
      package-gnupghome-dir (expand-file-name "gnupg" package-user-dir)
      package-quickstart t
      package-quickstart-file (expand-file-name "package-quickstart.el" minemacs-local-dir)
      package-vc-register-as-project nil
      use-package-verbose (cond (minemacs-debug-p 'debug) (minemacs-verbose-p t))
      use-package-always-defer (not minemacs-always-demand-p) ; defer loading packages, unless we use `:demand'
      use-package-always-demand minemacs-always-demand-p
      use-package-expand-minimally (not minemacs-debug-p)
      use-package-vc-prefer-newest t) ; prefer the newest commit over the latest release


;;; `use-package' extensions
;; Add `:vc' support for `use-package' in Emacs 29
(unless (> emacs-major-version 29) (require 'me-backports))

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

(package-initialize)

;;; Extra utilities
;; Be cautious about the installed revision of `once' and `satch' as they aren't stable yet
(use-package once
  :vc (:url "https://github.com/emacs-magus/once" :rev "a6f950c29c846a50018bc63695f24f611c1a58be" :ignored-files ("tests/*")))

(use-package satch
  :vc (:url "https://github.com/emacs-magus/satch.el" :rev "77993b711cccf16702fdc8d21d8f8ba10d7bd0fb"))


(provide 'me-bootstrap)
;;; me-bootstrap.el ends here
