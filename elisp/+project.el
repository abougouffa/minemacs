;; -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")


(defcustom +project-scan-dir-paths nil
  "A list of paths to scan and add to known projects list.
It can be a list of strings (paths) or a list of (cons \"~/path/to/projects\" recursive?)
to scan directories recursively."
  :group 'minemacs
  :type '(repeat (choice directory (cons directory boolean))))

;;;###autoload
(defun +project-scan-for-projects ()
  "Scan and remember projects under `+project-scan-dir-paths'."
  (interactive)
  (dolist (cons-dir +project-scan-dir-paths)
    (let* ((cons-dir (ensure-list cons-dir))
           (root-dir (car cons-dir))
           (recursive (cdr cons-dir))
           (sub-dirs (+directory-subdirs root-dir)))
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

