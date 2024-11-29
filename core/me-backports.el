;;; me-backports.el --- Port the `use-package's `:vc' feature for Emacs 29 -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(when (> emacs-major-version 29)
  (error "This module should only be used from Emacs 29"))

(require 'use-package)
(require 'package-vc)

(add-to-list 'use-package-keywords :vc)

(defvar use-package-vc-prefer-newest nil)

(defun use-package-vc-install (arg &optional local-path)
  (pcase-let* ((`(,name ,opts ,rev) arg)
               (spec (if opts (cons name opts) name)))
    (unless (package-installed-p name)
      (if local-path
          (package-vc-install-from-checkout local-path (symbol-name name))
        (package-vc-install spec rev)))))

(defun use-package-handler/:vc (name _keyword arg rest state)
  (let ((body (use-package-process-keywords name rest state))
        (local-path (car (plist-get state :load-path))))
    ;; See `use-package-handler/:ensure' for an explanation.
    (if (bound-and-true-p byte-compile-current-file)
        (funcall #'use-package-vc-install arg local-path)        ; compile time
      (push `(use-package-vc-install ',arg ,local-path) body))   ; runtime
    body))

(defconst use-package-vc-valid-keywords '(:url :branch :lisp-dir :main-file :vc-backend :rev :shell-command :make :ignored-files))

(defun use-package-normalize--vc-arg (arg)
  (cl-flet* ((ensure-string (s)
               (if (and s (stringp s)) s (symbol-name s)))
             (ensure-symbol (s)
               (if (and s (stringp s)) (intern s) s))
             (normalize (k v)
               (pcase k
                 (:rev (pcase v
                         ('nil (if use-package-vc-prefer-newest nil :last-release))
                         (:last-release :last-release)
                         (:newest nil)
                         (_ (ensure-string v))))
                 (:vc-backend (ensure-symbol v))
                 (:ignored-files (if (listp v) v (list v)))
                 (_ (ensure-string v)))))
    (pcase-let* ((`(,name . ,opts) arg))
      (if (stringp opts)                ; (NAME . VERSION-STRING) ?
          (list name opts)
        (let ((opts (use-package-split-when
                     (lambda (el)
                       (seq-contains-p use-package-vc-valid-keywords el))
                     opts)))
          ;; Error handling
          (cl-loop for (k . _) in opts
                   if (not (member k use-package-vc-valid-keywords))
                   do (use-package-error
                       (format "Keyword :vc received unknown argument: %s. Supported keywords are: %s"
                               k use-package-vc-valid-keywords)))
          ;; Actual normalization
          (list name
                (cl-loop for (k . v) in opts
                         if (not (eq k :rev))
                         nconc (list k (normalize k (if (length= v 1) (car v) v))))
                (normalize :rev (car (alist-get :rev opts)))))))))

(defun use-package-normalize/:vc (name _keyword args)
  (let ((arg (car args)))
    (pcase arg
      ((or 'nil 't) (list name))                 ; guess name
      ((pred symbolp) (list arg))                ; use this name
      ((pred stringp) (list name arg))           ; version string + guess name
      (`(,(pred keywordp) . ,(pred listp))       ; list + guess name
       (use-package-normalize--vc-arg (cons name arg)))
      (`(,(pred symbolp) . ,(or (pred listp)     ; list/version string + name
                             (pred stringp)))
       (use-package-normalize--vc-arg arg))
      (_ (use-package-error "Unrecognized argument to :vc")))))


(provide 'me-backports)
;;; me-backports.el ends here
