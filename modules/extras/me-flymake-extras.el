;;; me-flymake-extras.el --- Additional backends for Flymake using `flymake-collection-define' -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(require 'flymake-collection-define)

;; Custom variables for `flymake-collection-clang-tidy'
(defcustom flymake-collection-clang-tidy-build-path "build"
  "Clang build directory."
  :type '(choice (const nil) directory)
  :group 'minemacs-prog)

(defcustom flymake-collection-clang-tidy-extra-options nil
  "Extra options to pass to Clang-tidy."
  :type '(choice (const nil) (repeat string))
  :group 'minemacs-prog)

;; Custom variables for `flymake-collection-nasm'
(defcustom flymake-collection-nasm-format "elf64"
  "The NASM output format."
  :type 'string
  :group 'minemacs-prog)

;;; Helpers

;; Helper functions for `flymake-clang-tidy'
(defun flymake-clang-tidy-find-project-root (_checker)
  "Find the project root for CHECKER.
This uses `project', `projectile', `vc' or the \".clang-tidy\" file"
  (or
   (and (project-current) (project-root (project-current)))
   (when (and (featurep 'projectile) (bound-and-true-p projectile-mode)) (projectile-project-root))
   (vc-root-dir)
   (locate-dominating-file (or buffer-file-name default-directory) ".clang-tidy")
   (progn
     (message "Could not determine project root, trying current directory.")
     (file-name-directory buffer-file-name))))

(defun flymake-collection-clang-tidy-get-config ()
  "Find and read .clang-tidy."
  (when-let* ((config-dir (locate-dominating-file (or buffer-file-name default-directory) ".clang-tidy"))
              (config-file (expand-file-name ".clang-tidy" config-dir)))
    (with-temp-buffer
      (insert-file-contents config-file)
      (buffer-string))))

;;; Checkers

;; Inspired by `flymake-nasm' - https://github.com/juergenhoetzel/flymake-nasm
;;;###autoload (autoload 'flymake-collection-nasm "me-flymake-extras")
(flymake-collection-define-rx flymake-collection-nasm
  "Assembly checker using the Netwide Assembler (NASM)."
  :title "nasm"
  :pre-let ((nasm-exec (executable-find "nasm")))
  :pre-check (unless nasm-exec (error "Not found nasm on PATH"))
  :write-type 'file
  :command `(,nasm-exec ,(concat "-f" flymake-collection-nasm-format) ,flymake-collection-temp-file)
  :regexps
  ((error bol (file-name) ":" line ": error: " (message) eol)
   (warning bol (file-name) ":" line ": warning: " (message) eol)
   (note bol (file-name) ":" line ": note: " (message) eol)))

;; Inspired by `flymake-pyre' - https://github.com/juergenhoetzel/flymake-pyre
;;;###autoload (autoload 'flymake-collection-pyre "me-flymake-extras")
(flymake-collection-define-rx flymake-collection-pyre
  "Performant type-checking for python."
  :title "pyre"
  :pre-let ((pyre-exec (executable-find "pyre")))
  :pre-check (unless pyre-exec (error "Cannot find pyre in PATH"))
  :write-type 'file
  :command `(,pyre-exec)
  :regexps
  ((warning bol (file-name) ":" line ":" column " " (message) eol)))

;;;###autoload (autoload 'flymake-collection-codespell "me-flymake-extras")
(flymake-collection-define-rx flymake-collection-codespell
  "Check code for common misspellings."
  :title "codespell"
  :pre-let ((codespell-exec (executable-find "codespell")))
  :pre-check (unless codespell-exec (error "Cannot find codespell executable"))
  :write-type 'file
  :command (list codespell-exec "-d" "-i0" flymake-collection-temp-file)
  :regexps
  ((warning bol (file-name) ":" line ": " (message) eol)))

;; Inspired by `flymake-quickdef' example - https://github.com/karlotness/flymake-quickdef
;;;###autoload (autoload 'flymake-collection-bandit "me-flymake-extras")
(flymake-collection-define-rx flymake-collection-bandit
  "Find common security issues in Python code."
  :title "bandit"
  :pre-let ((bandit-exec (executable-find "bandit")))
  :pre-check (unless bandit-exec (error "Cannot find bandit executable"))
  :write-type 'file
  :command (list bandit-exec "--format" "custom" "--msg-template" "diag:{line}:{severity}:{test_id}: {msg}" flymake-collection-temp-file)
  :regexps
  ((error   bol "diag:" line ":" "HIGH" ":" (id (* alnum)) ":" (message) eol)
   (warning bol "diag:" line ":" "MEDIUM" ":" (id (* alnum)) ":" (message) eol)
   (note    bol "diag:" line ":" (or "LOW" "UNDEFINED") ":" (id (* alnum)) ":" (message) eol)))

;; Inspired by `flycheck-clang-tidy' - https://github.com/ch1bo/flycheck-clang-tidy
;;;###autoload (autoload 'flymake-collection-clang-tidy "me-flymake-extras")
(flymake-collection-define-rx flymake-collection-clang-tidy
  "Clang-based C++ linter tool."
  :pre-let ((clang-tidy-exec (executable-find "clang-tidy")))
  :pre-check (unless clang-tidy-exec (error "Cannot find clang-tidy executable"))
  :write-type 'file
  :command (append
            (list clang-tidy-exec)
            (when flymake-collection-clang-tidy-build-path (list "-p" flymake-collection-clang-tidy-build-path))
            (when buffer-file-name (list (concat "-extra-arg=-I" (file-name-directory buffer-file-name))))
            (when (flymake-collection-clang-tidy-get-config) (list (concat "-config=" (flymake-collection-clang-tidy-get-config))))
            (ensure-list flymake-collection-clang-tidy-extra-options)
            (list flymake-collection-temp-file))
  :regexps
  ((error bol (file-name) ":" line ":" column ": error:" (message) eol)
   (warning bol (file-name) ":" line ":" column ": warning:" (message) eol)
   (note bol (file-name) ":" line ":" column ": note:" (message) eol)))


(provide 'me-flymake-extras)

;;; me-flymake-extras.el ends here
