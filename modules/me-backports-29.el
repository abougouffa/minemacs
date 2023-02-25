;;; me-backports-29.el --- Some Emacs 29 functionalities ported back to Emacs 28 -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

;; MinEmacs is distributed under the MIT license. However, this file is mostly a
;; copy-and-paste from Emacs 29 with some adaptations, hence, it is licensed
;; with original Emacs GNU GPL-3.0 license.

(require 'wid-edit) ;; Needed by `setopt--set'

(defun scratch-buffer ()
  "Switch to the *scratch* buffer.
If the buffer doesn't exist, create it first."
  (interactive)
  (pop-to-buffer-same-window (get-scratch-buffer-create)))

(defun get-scratch-buffer-create ()
  "Return the *scratch* buffer, creating a new one if needed."
  (or (get-buffer "*scratch*")
      (let ((scratch (get-buffer-create "*scratch*")))
        ;; Don't touch the buffer contents or mode unless we know that
        ;; we just created it.
        (with-current-buffer scratch
          (when initial-scratch-message
            (insert (substitute-command-keys initial-scratch-message))
            (set-buffer-modified-p nil))
          (funcall initial-major-mode))
        scratch)))

(defun startup-redirect-eln-cache (cache-directory)
  "Redirect the user's eln-cache directory to CACHE-DIRECTORY.
CACHE-DIRECTORY must be a single directory, a string.
This function destructively changes `native-comp-eln-load-path'
so that its first element is CACHE-DIRECTORY.  If CACHE-DIRECTORY
is not an absolute file name, it is interpreted relative
to `user-emacs-directory'.
For best results, call this function in your early-init file,
so that the rest of initialization and package loading uses
the updated value."
  ;; Remove the original eln-cache.
  (setq native-comp-eln-load-path (cdr native-comp-eln-load-path))
  ;; Add the new eln-cache.
  (push (expand-file-name (file-name-as-directory cache-directory)
                          user-emacs-directory)
        native-comp-eln-load-path))

(defalias 'native-comp-jit-compilation 'inhibit-automatic-native-compilation)

(defalias 'string-split #'split-string)

(defalias 'loaddefs-generate #'make-directory-autoloads)

(defmacro with-memoization (place &rest code)
  "Return the value of CODE and stash it in PLACE.
If PLACE's value is non-nil, then don't bother evaluating CODE
and return the value found in PLACE instead."
  (declare (indent 1) (debug (gv-place body)))
  (gv-letplace (getter setter) place
    `(or
      ,getter
      ,(macroexp-let2 nil val (macroexp-progn code)
        `(progn
           ,(funcall setter val)
           ,val)))))

(defun native-compile-prune-cache ()
  "Remove .eln files that aren't applicable to the current Emacs invocation."
  (interactive)
  (unless (featurep 'native-compile)
    (user-error "This Emacs isn't built with native-compile support"))
  ;; The last item in native-comp-eln-load-path is assumed to be a system
  ;; directory, so don't try to delete anything there (bug#59658).
  (dolist (dir (butlast native-comp-eln-load-path))
    ;; If a directory is non absolute it is assumed to be relative to
    ;; `invocation-directory'.
    (setq dir (expand-file-name dir invocation-directory))
    (when (file-exists-p dir)
      (dolist (subdir (seq-filter
                       (lambda (f) (not (string-match (rx "/." (? ".") eos) f)))
                       (directory-files dir t)))
        (when (and (file-directory-p subdir)
                   (file-writable-p subdir)
                   (not (equal (file-name-nondirectory
                                (directory-file-name subdir))
                               comp-native-version-dir)))
          (message "Deleting `%s'..." subdir)
          ;; We're being overly cautious here -- there shouldn't be
          ;; anything but .eln files in these directories.
          (dolist (eln (directory-files subdir t "\\.eln\\(\\.tmp\\)?\\'"))
            (when (file-writable-p eln)
              (delete-file eln)))
          (when (directory-empty-p subdir)
            (delete-directory subdir))))))
  (message "Cache cleared"))

(defmacro setopt (&rest pairs)
  "Set VARIABLE/VALUE pairs, and return the final VALUE.
This is like `setq', but is meant for user options instead of
plain variables.  This means that `setopt' will execute any
`custom-set' form associated with VARIABLE."
  (declare (debug setq))
  (unless (zerop (mod (length pairs) 2))
    (error "PAIRS must have an even number of variable/value members"))
  (let ((expr nil))
    (while pairs
      (unless (symbolp (car pairs))
        (error "Attempting to set a non-symbol: %s" (car pairs)))
      (push `(setopt--set ',(car pairs) ,(cadr pairs))
            expr)
      (setq pairs (cddr pairs)))
    (macroexp-progn (nreverse expr))))

(defun setopt--set (variable value)
  (custom-load-symbol variable)
  ;; Check that the type is correct.
  (when-let ((type (get variable 'custom-type)))
    (unless (widget-apply (widget-convert type) :match value)
      (warn "Value `%S' does not match type %s" value type)))
  (put variable 'custom-check-value (list value))
  (funcall (or (get variable 'custom-set) #'set-default) variable value))


(provide 'me-backports-29)
