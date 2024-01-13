;;; me-flycheck-eglot.el --- Hacky eglot support in flycheck (from Doom Emacs) -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:
;; This file sets up flycheck so that, when eglot receives a publishDiagnostics
;; method from the server, flycheck updates the reports.
;;
;; Thanks to:
;; - joaotavora for adding a handle to plug flycheck, and
;; - purcell for finding out the initial stub and the current implementation
;;
;; It works by creating a bridge function which can be used as the argument of
;; `eglot-flymake-backend', which both consumes diagnostics and queue a call to
;; 'flycheck-buffer'

;;; Code:

;; Adapted from Doom Emacs

(require 'eglot)
(require 'flycheck)

(defvar-local +flycheck-eglot--current-errors nil)

(defun +flycheck-eglot--init (_checker callback)
  "CHECKER is the checker (eglot).
CALLBACK is the function that we need to call when we are done,
on all the errors."
  (eglot-flymake-backend #'+flycheck-eglot--on-diagnostics)
  (funcall callback 'finished +flycheck-eglot--current-errors))

(defun +flycheck-eglot--on-diagnostics (diags &rest _)
  "Bind Flycheck diagnostics DIAGS to Flymake's."
  (cl-labels
      ((flymake-diag->flycheck-err
         (diag)
         (with-current-buffer (flymake--diag-buffer diag)
           (flycheck-error-new-at-pos
            (flymake--diag-beg diag)
            (pcase (flymake--diag-type diag)
              ('eglot-note 'info)
              ('eglot-warning 'warning)
              ('eglot-error 'error)
              (_ (error "Unknown diagnostic type, %S" diag)))
            (flymake--diag-text diag)
            :end-pos (flymake--diag-end diag)
            :checker 'eglot
            :buffer (current-buffer)
            :filename (buffer-file-name)))))
    (setq +flycheck-eglot--current-errors
          (mapcar #'flymake-diag->flycheck-err diags))
    ;; Call Flycheck to update the diagnostics annotations
    (flycheck-buffer-deferred)))

(defun +flycheck-eglot--available-p ()
  "Return non-nil if the current buffer is managed by some Eglot project."
  (bound-and-true-p eglot--managed-mode))

(flycheck-define-generic-checker
    'eglot
  "Report `eglot' diagnostics using `flycheck'."
  :start #'+flycheck-eglot--init
  :predicate #'+flycheck-eglot--available-p
  :modes '(prog-mode text-mode))

(push 'eglot flycheck-checkers)

;;;###autoload
(defun +eglot-setup-flycheck ()
  "Prefer Flycheck over Flymake to use with Eglot."
  (interactive)
  (when eglot--managed-mode
    (flymake-mode -1)
    (when-let ((current-checker (flycheck-get-checker-for-buffer)))
      (unless (equal current-checker 'eglot)
        (flycheck-add-next-checker 'eglot current-checker)
        (flycheck-add-mode 'eglot major-mode)))
    (unless (bound-and-true-p +flycheck-disabled-explicitly)
      (flycheck-mode 1)
      ;; Call flycheck on initilization to make sure to display initial
      ;; errors
      (flycheck-buffer-deferred))))

(add-hook 'eglot-managed-mode-hook #'+eglot-setup-flycheck)

(with-eval-after-load 'flymake
  (when (and (not (fboundp 'flymake--diag-buffer))
             (fboundp 'flymake--diag-locus))
    (defalias 'flymake--diag-buffer 'flymake--diag-locus)))


(provide 'obsolete/me-flycheck-eglot)

;;; me-flycheck-eglot.el ends here
