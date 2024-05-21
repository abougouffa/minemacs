;;; me-flymake-quickdef.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package flymake-quickdef
  :straight t
  :autoload flymake-quickdef-backend +flymake-bandit-load +flymake-codespell-load +flymake-clang-tidy-load
  :init
  ;; Automatically generate `backend-load' function to be used as a hook
  (advice-add
   'flymake-quickdef-backend :after
   (satch-defun +flymake-quickdef--make-load-fn:after-a (backend &rest _)
     (defalias (intern (format "+%s-load" backend))
       (lambda () (add-hook 'flymake-diagnostic-functions backend nil t))
       (format "Load %s as a Flymake backend in the current buffer." backend))))

  (when (executable-find "bandit")
    (+add-hook! (python-mode python-ts-mode) #'+flymake-bandit-load))

  (when (executable-find "codespell")
    (+add-hook! prog-mode #'+flymake-codespell-load))

  ;; Custom variables for `flymake-clang-tidy'
  (defcustom flymake-clang-tidy-build-path "build"
    "Clang build directory."
    :type '(choice (const nil) directory)
    :group 'minemacs-prog)
  (defcustom flymake-clang-tidy-extra-options nil
    "Extra options to pass to Clang-tidy."
    :type '(choice (const nil) (repeat string))
    :group 'minemacs-prog)
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
      (list fmqd-source beg end type msg)))

  ;; Codespell backend from https://github.com/chmouel/flymake-codespell
  (flymake-quickdef-backend flymake-codespell
    :pre-let ((codespell-exec (executable-find "codespell")))
    :pre-check (unless codespell-exec (error "Cannot find codespell executable"))
    :write-type 'file
    :proc-form (list codespell-exec "-d" "-i0" fmqd-temp-file)
    :search-regexp "^\\([[:alnum:][:punct:]]+\\):\\([[:digit:]]+\\):[[:space:]]\\(.+\\)"
    :prep-diagnostic
    (let* ((lnum (string-to-number (match-string 2)))
           (text (match-string 3))
           (pos (flymake-diag-region fmqd-source lnum))
           (beg (car pos))
           (end (cdr pos))
           (type :warning)
           (msg text))
      (list fmqd-source beg end type msg)))

  ;; Define backend for Clang-tidy (inspired by `flycheck-clang-tidy')
  (flymake-quickdef-backend flymake-clang-tidy
    :pre-let ((clang-tidy-exec (executable-find "clang-tidy")))
    :pre-check (unless clang-tidy-exec (error "Cannot find clang-tidy executable"))
    :write-type 'file
    :proc-form (append
                (list clang-tidy-exec)
                (when flymake-clang-tidy-build-path (list "-p" flymake-clang-tidy-build-path))
                (when buffer-file-name (list (concat "-extra-arg=-I" (file-name-directory buffer-file-name))))
                (when (flymake-clang-tidy-get-config) (list (concat "-config=" (flymake-clang-tidy-get-config))))
                (ensure-list flymake-clang-tidy-extra-options)
                (list fmqd-temp-file))
    :search-regexp "^\\([[:alnum:][:punct:]]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\): \\(\\(?:error\\|note\\|warning\\)\\): \\(.*\\)$"
    :prep-diagnostic
    (let* ((file (match-string 1))
           (lnum (string-to-number (match-string 2)))
           (lcol (string-to-number (match-string 3)))
           (severity (match-string 4))
           (type (cond ((string= severity "warning") :warning)
                       ((string= severity "error") :error)
                       (t :note)))
           (msg (match-string 5))
           (pos (flymake-diag-region fmqd-source lnum))
           (beg (car pos))
           (end (cdr pos)))
      (list fmqd-source beg end type msg)))

  ;; Helper functions for `flymake-clang-tidy'
  (defun flymake-clang-tidy-find-project-root (_checker)
    "Find the project root for CHECKER.
This uses `project', `projectile', `vc' or the \".clang-tidy\" file"
    (or
     (and (project-current) (project-root (project-current)))
     (when (and (featurep 'projectile) (bound-and-true-p projectile-mode)) (projectile-project-root))
     (vc-root-dir)
     (+directory-root-containing-file ".clang-tidy")
     (progn
       (message "Could not determine project root, trying current directory.")
       (file-name-directory buffer-file-name))))

  (defun flymake-clang-tidy-get-config ()
    "Find and read .clang-tidy."
    (when-let* ((config-dir (+directory-root-containing-file ".clang-tidy"))
                (config-file (expand-file-name ".clang-tidy" config-dir)))
      (with-temp-buffer
        (insert-file-contents config-file)
        (buffer-string)))))


;; Backends that depends on `flymake-quickdef'

(use-package flymake-nasm
  :straight (:host github :repo "juergenhoetzel/flymake-nasm"))

(use-package flymake-pyre
  :straight (:host github :repo "juergenhoetzel/flymake-pyre")
  :init
  (when (executable-find "pyre") (add-hook 'asm-mode-hook #'flymake-pyre-setup)))


(provide 'obsolete/me-flymake-quickdef)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; me-flymake-quickdef.el ends here
