;;; me-checkers.el --- Syntax checking -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package flymake-collection
  :straight t
  :hook (minemacs-after-startup . flymake-collection-hook-setup)
  :config
  ;; Activate more checkers for Python
  (setf (alist-get '(python-mode python-ts-mode) flymake-collection-hook-config nil nil 'equal)
        (append (when (executable-find "pycodestyle") '(flymake-collection-pycodestyle))
                (when (executable-find "mypy") '(flymake-collection-mypy))
                (when (executable-find "pylint") '(flymake-collection-pylint))
                (when (executable-find "ruff") '(flymake-collection-ruff))
                '((flymake-collection-flake8 :disabled t)))))

(use-package flymake-cppcheck
  :straight (:host github :repo "shaohme/flymake-cppcheck")
  :init
  (when (executable-find "cppcheck")
    (+add-hook! (c-mode c-ts-mode c++-mode c++-ts-mode) #'flymake-cppcheck-setup)))

(use-package flymenu
  :straight (:host github :repo "KarimAziev/flymenu")
  :init
  (+map! "cM" #'flymenu-flymake))

(use-package flymake-guile
  :straight t
  :init
  (when (executable-find "guild") (+add-hook! scheme-mode #'flymake-guile)))

(use-package flymake-quickdef
  :straight t
  :autoload flymake-quickdef-backend +flymake-bandit-load +flymake-codespell-load
  :init
  ;; Automatically generate `backend-load' function to be used as a hook
  (advice-add
   'flymake-quickdef-backend :after
   (defun +flymake-quickdef--make-load-fn:after-a (backend &rest _)
     (let ((fn (intern (format "+%s-load" backend))))
       (defalias fn
         (lambda () (add-hook 'flymake-diagnostic-functions backend nil t))))))

  (when (executable-find "bandit")
    (+add-hook! (python-mode python-ts-mode) #'+flymake-bandit-load))

  (when (executable-find "codespell")
    (+add-hook! prog-mode #'+flymake-codespell-load))
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
      (list fmqd-source beg end type msg))))

(use-package flymake-plantuml
  :straight (:host github :repo "shaohme/flymake-plantuml")
  :hook (plantuml-mode . flymake-plantuml-setup))

(use-package flymake-relint
  :straight t
  :hook ((emacs-lisp-mode lisp-interaction-mode) . flymake-relint-setup))

(use-package flymake-pmd
  :straight (:host github :repo "rody/flymake-pmd"))

(use-package flymake-nasm
  :straight (:host github :repo "juergenhoetzel/flymake-nasm")
  :init
  (when (executable-find "nasm") (+add-hook! asm-mode #'flymake-nasm-setup)))

(use-package flymake-pyre
  :straight (:host github :repo "juergenhoetzel/flymake-pyre")
  :init
  (when (executable-find "pyre") (+add-hook! asm-mode #'flymake-pyre-setup)))


(provide 'me-checkers)

;;; me-checkers.el ends here
