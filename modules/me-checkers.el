;;; me-checkers.el --- Syntax checking -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package flymake-collection
  :straight t
  :hook (minemacs-after-startup . flymake-collection-hook-setup))

(use-package flymake-cppcheck
  :straight (nil :host github :repo "shaohme/flymake-cppcheck")
  :hook ((c-mode c-ts-mode c++-mode c++-ts-mode) . flymake-cppcheck-setup))

(use-package flymake-guile
  :straight t
  :hook (scheme-mode . flymake-guile))

(use-package flymake-quickdef
  :straight t
  :autoload flymake-quickdef-backend
  :hook ((python-mode python-ts-mode) . +flymake-bandit-load)
  :hook (prog-mode . +flymake-codespell-load)
  :init
  ;; Automatically generate `backend-load' function to be used as a hook
  (advice-add
   'flymake-quickdef-backend :after
   (defun +flymake-quickdef--make-load-fn (backend &rest _)
     (let ((fn (intern (format "+%s-load" backend))))
       (defalias fn
         (lambda () (add-hook 'flymake-diagnostic-functions backend nil t))))))
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
    :prep-diagnostic
    (let* ((lnum (string-to-number (match-string 2)))
           (text (match-string 3))
           (pos (flymake-diag-region fmqd-source lnum))
           (beg (car pos))
           (end (cdr pos))
           (type :warning)
           (msg text))
      (list fmqd-source beg end type msg))
    :search-regexp
    (rx bol (group (one-or-more (any alnum punct)))
        ":" (group (one-or-more digit))
        ":" space (group (one-or-more any)))))

(use-package flymake-plantuml
  :straight (:host github :repo "shaohme/flymake-plantuml")
  :hook (plantuml-mode . flymake-plantuml-setup))


(provide 'me-checkers)

;;; me-checkers.el ends here
