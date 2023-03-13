;; -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")


(defcustom +project-scan-dir-paths nil
  "A list of paths to scan and add to known projects list.
It can be a list of strings (paths) or a list of (cons \"~/path/to/projects\" recursive?)
to scan directories recursively.")

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
