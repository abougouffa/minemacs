;;; me-git.el --- Some Git extas -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-06-22
;; Last modified: 2025-06-25

;;; Commentary:

;;; Code:

;; Inspired by https://github.com/jesse-c/dotfiles/blob/7788769fc48caffc600409e05be04669c16a71c4/home/dot_config/emacs/init.el#L295
;;;###autoload
(defcustom +git-commit-prefix-in-project nil
  "Propose to insert a commit prefix in the current project.

Set this variable in your project's \".dir-locals.el\". You can set it
to `conventional' or `prefix'."
  :group 'minemacs-vc
  :type '(choice (const conventional) (const prefix) (const nil)))

;;;###autoload
(make-variable-buffer-local '+git-commit-prefix-in-project)

(defvar +git-types-cache-age (* 60 60 24)) ; 24h
(defvar +git--types nil)
(defvar +git-commit-format-alist
  `((conventional
     . ((type . ,(rx (seq bol (group-n 1 (+ lower)) (any "(" ":" "!" "+") (* any))))
        (scope . (rx-to-string `(seq bol (or ,@+git--types) "(" (group-n 1 (+ (not ")"))) ")")))
        (scope-sep . ",")
        (format . ("%s%s: " . "(%s)"))))
    (prefix
     . ((type . ,(rx (seq bol "[" (group-n 1 (+ (not (any "]" space)))) (* any))))
        (scope . ,(rx (seq bol "[" (+ (not (any "]" space))) (group-n 1 (+ (not (any "]")))))))
        (scope-sep . " ")
        (format . ("[%s%s] " . " %s"))))))

(defun +git--types-cache-file (conv kind)
  "Cache file for CONV (`conventional' or `prefix') and KIND (`types' or `scopes').
Returns a cons cell \\=(filename . valid-p) with its car the cache file
name and its cdr indicates if the cache file is still valid."
  (let* ((cache-dir (expand-file-name ".cache" (or (+project-safe-root) default-directory)))
         (cache-file (expand-file-name (format "%s-%s" conv kind) cache-dir)))
    (unless (file-exists-p cache-dir) (make-directory cache-dir t))
    (cons cache-file
          (and (file-exists-p cache-file)
               (< (time-to-seconds (time-since (file-attribute-modification-time (file-attributes cache-file))))
                  +git-types-cache-age)))))

(defun +git-get-commit-kind (conv kind)
  "CONV can be `conventional' or `prefix', KIND can be `types' or `scopes'."
  (let* ((default-directory (magit-toplevel))
         (cache-file (+git--types-cache-file conv kind))
         (scopes-p (eq kind 'scopes)))
    (unless (cdr cache-file)
      (with-temp-buffer
        (call-process "git" nil t nil "log" "--pretty=format:%s")
        (goto-char (point-min))
        (let-alist (alist-get conv +git-commit-format-alist)
          (let* ((+git--types (when scopes-p (+git-get-commit-kind conv 'types)))
                 (elements (cl-loop
                            while (let ((case-fold-search nil))
                                    (re-search-forward (eval (if scopes-p .scope .type)) nil 'noerror))
                            append (if scopes-p
                                       (string-split (match-string 1) .scope-sep t "[[:space:]]")
                                     (list (match-string 1))))))
            (with-temp-file (car cache-file) ; Write unique scopes to the cache file
              (insert (string-join (delete-dups elements) "\n")))))))
    (when (file-exists-p (car cache-file))
      (with-temp-buffer
        (insert-file-contents (car cache-file))
        (split-string (buffer-string) "\n" t)))))

;;;###autoload
(defun +git-insert-commit-prefix (&optional clean-cache)
  "Prompt for conventional commit type with scope completion.
When CLEAN-CACHE is non-nil, regenerate the cache, otherwise, the cache
will be generated each `+git-types-cache-age'."
  (interactive "P")
  (let ((conv +git-commit-prefix-in-project)
        (+git-types-cache-age (if clean-cache 0 +git-types-cache-age)))
    (when (and conv
               (or (interactive-p)
                   (and (+first-line-empty-p) ; Skip when amending a commit
                        (y-or-n-p (format "Use %s commit format? " (symbol-name conv))))))
      (let-alist (alist-get conv +git-commit-format-alist)
        (let* ((type (completing-read "Commit type: " (+git-get-commit-kind conv 'types)))
               (known-scopes (+git-get-commit-kind conv 'scopes))
               (scopes (and known-scopes (completing-read-multiple "Scope: " known-scopes))))
          (insert (format (car .format) type
                          (let ((s (string-join scopes .scope-sep)))
                            (if (string-empty-p s) "" (format (cdr .format) s)))))
          (let ((pos (point))) (insert "\n") (goto-char pos)))))))


(provide 'me-git)
;;; me-git.el ends here
