;;; me-cscope.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@tznvy.pbz")
;; Created: 2025-07-09
;; Last modified: 2025-07-09

;;; Commentary:

;;; Code:


;; Cscope interface for Emacs
(use-package xcscope
  :straight t
  :unless (featurep 'os/win)
  :commands (cscope-create-list-of-files-to-index cscope-index-files)
  :custom
  (cscope-option-do-not-update-database t)
  (cscope-display-cscope-buffer nil))


;; Cscope integration for Emacs' Consult
(use-package consult-cscope
  :straight (:host github :repo "blorbx/consult-cscope")
  :unless (+package-disabled-p 'consult 'me-completion)
  :config
  (defun +consult--cscope-find-database-file (start-dir)
    "Looks first for the dominating directory that includes the database file.
Fallback to the default function if none is found."
    (if-let* ((dir (locate-dominating-file start-dir consult-cscope-database-file))
              (not-abs-path (not (file-name-absolute-p consult-cscope-database-file))))
        (expand-file-name consult-cscope-database-file dir)
      (consult--cscope-find-database-file start-dir)))

  ;; Use my modified database finder, particularly useful in workspaces with
  ;; multiple projects (a.k.a. super-projects)
  (setq consult--cscope-database-finder #'+consult--cscope-find-database-file)

  ;; Use `+region-or-thing-at-point' for initial input
  (consult-customize
   consult-cscope-file consult-cscope-calling consult-cscope-called-by
   consult-cscope-text consult-cscope-egrep consult-cscope-symbol
   consult-cscope-including consult-cscope-assignment consult-cscope-definition
   :initial (+region-or-thing-at-point)))


(provide 'obsolete/me-cscope)
;;; me-cscope.el ends here
