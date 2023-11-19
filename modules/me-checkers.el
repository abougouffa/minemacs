;;; me-checkers.el --- Syntax checking -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package flymake-ruff
  :straight t
  :hook ((python-mode python-ts-mode) . flymake-ruff-load))

(use-package flymake-collection
  :straight t
  :hook (minemacs-after-startup . flymake-collection-hook-setup))

(use-package flymake-quickdef
  :straight t
  :autoload flymake-quickdef-backend
  :hook ((python-mode python-ts-mode) . +flymake-bandit-load)
  :init
  (defun +flymake-bandit-load ()
    (add-hook 'flymake-diagnostic-functions #'flymake-check-bandit nil t))
  :config
  ;; Add Bandit support for Python (example from https://github.com/karlotness/flymake-quickdef)
  (flymake-quickdef-backend flymake-bandit
    :pre-let ((bandit-exec (executable-find "bandit")))
    :pre-check (unless bandit-exec (error "Cannot find bandit executable"))
    :write-type 'file
    :proc-form (list bandit-exec "--format" "custom" "--msg-template" "diag:{line} {severity} {test_id}: {msg}" fmqd-temp-file)
    :search-regexp "^diag:\\([[:digit:]]+\\) \\(HIGH\\|LOW\\|MEDIUM\\|UNDEFINED\\) \\([[:alpha:]][[:digit:]]+\\): \\(.*\\)$"
    :prep-diagnostic
    (let* ((lnum (string-to-number (match-string 1)))
           (severity (match-string 2))
           (code (match-string 3))
           (text (match-string 4))
           (pos (flymake-diag-region fmqd-source lnum))
           (beg (car pos))
           (end (cdr pos))
           (type (cond
                  ((string= severity "HIGH") :error)
                  ((string= severity "MEDIUM") :warning)
                  (t :note)))
           (msg (format "%s (%s)" text code)))
      (list fmqd-source beg end type msg))))


(provide 'me-checkers)

;;; me-checkers.el ends here
