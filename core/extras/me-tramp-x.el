;;; me-tramp.el --- Tramp memoization -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-06-25
;; Last modified: 2025-06-27

;;; Commentary:

;;; Code:

(defun +memoize-remote (file cache orig-fn &rest args)
  "Memoize the value of calling ORIG-FN on ARGS in CACHE if FILE is remote."
  (if (and file (file-remote-p file))
      (if-let* ((current (assoc file (symbol-value cache))))
          (cdr current)
        (when-let* ((current (apply orig-fn args)))
          (set cache (cons (cons file current) (symbol-value cache)))
          current))
    (apply orig-fn args)))

;; Memoize current project
(defvar +project-current-cache nil)

;;;###autoload
(defun +memoize-project-current (orig &optional prompt directory)
  (+memoize-remote
   (or directory project-current-directory-override default-directory)
   '+project-current-cache orig prompt directory))

;;;###autoload
(advice-add 'project-current :around #'+memoize-project-current)

;; Memoize magit top level
(defvar +magit-toplevel-cache nil)

;;;###autoload
(defun +memoize-magit-toplevel (orig &optional directory)
  (+memoize-remote (or directory default-directory) '+magit-toplevel-cache orig directory))

;;;###autoload
(advice-add 'magit-toplevel :around #'+memoize-magit-toplevel)

;; memoize vc-git-root
(defvar +vc-git-root-cache nil)

;;;###autoload
(defun +memoize-vc-git-root (orig file)
  (let ((value (+memoize-remote (file-name-directory file) '+vc-git-root-cache orig file)))
    ;; sometimes vc-git-root returns nil even when there is a root there
    (when (null (cdr (car +vc-git-root-cache)))
      (setq +vc-git-root-cache (cdr +vc-git-root-cache)))
    value))

;;;###autoload
(advice-add 'vc-git-root :around #'+memoize-vc-git-root)


(provide 'me-tramp)
;;; me-tramp.el ends here
