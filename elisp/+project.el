;; -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(defgroup minemacs-project nil
  "MinEmacs project stuff."
  :group 'minemacs)

(defcustom +project-scan-dir-paths nil
  "A list of paths to scan and add to known projects list.
It can be a list of strings (paths) or a list of (cons \"~/path/to/projects\" recursive?)
to scan directories recursively."
  :group 'minemacs-project
  :type '(repeat (choice directory (cons directory boolean))))

;;;###autoload
(defun +project-scan-for-projects (&optional dir)
  "Scan and remember projects under DIR or `+project-scan-dir-paths'."
  (interactive)
  (dolist (cons-dir (or dir +project-scan-dir-paths))
    (let* ((cons-dir (ensure-list cons-dir))
           (root-dir (car cons-dir))
           (recursive (cdr cons-dir))
           (sub-dirs (and (file-directory-p root-dir) (+directory-subdirs root-dir))))
      (dolist (dir sub-dirs)
        (project-remember-projects-under dir recursive)))))

;;;###autoload
(defun +project-add-project (dir &optional dont-ask)
  "Switch to another project at DIR.
When DIR is not detected as a project, ask to force it to be by adding a
\".project.el\" file."
  (interactive (list (project-prompt-project-dir)))
  (project-switch-project dir)
  (when (and (not (project-current))
             (or dont-ask
                 (yes-or-no-p "Directory not detected as a project, add \".project.el\"? ")))
    (with-temp-buffer
      (write-file (expand-file-name ".project.el" dir)))))

;;;###autoload
(defun +project-gdb ()
  "Invoke `gdb' in the project's root."
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (call-interactively #'gdb)))

;;;###autoload
(defun +project-list-cleanup ()
  "Forget all duplicate known projects (/home/user/proj, ~/proj)."
  (interactive)
  (let* ((projs (mapcar #'expand-file-name (project-known-project-roots)))
         (projs-dups (cl-set-difference projs (cl-remove-duplicates projs :test #'string=))))
    (mapc #'project-forget-project projs-dups)
    (project-forget-zombie-projects)
    (dolist (proj projs)
      (let ((proj-abbrev (abbreviate-file-name proj)))
        (unless (string= proj proj-abbrev)
          (project-forget-project proj)
          (project-remember-projects-under proj-abbrev))))))

;;; +project.el ends here
