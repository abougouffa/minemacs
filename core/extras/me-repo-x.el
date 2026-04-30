;;; me-repo-x.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@cneebg.pbz")
;; Created: 2026-04-30
;; Last modified: 2026-04-30

;;; Commentary:

;; Better integration of the Repo tool (https://gerrit.googlesource.com/git-repo)

;;; Code:


;;;###autoload
(defun +repo-root (&optional dir)
  "Return Repo's root directory starting from DIR."
  (when-let* ((start-point (or dir (buffer-file-name) default-directory))
              (repo-dir (locate-dominating-file start-point ".repo"))
              ((file-directory-p repo-dir)))
    (expand-file-name repo-dir)))

;;;###autoload
(defun +repo-projects (&rest exclude-prefixes)
  "Return the list of repo projects in the current directory.

When EXCLUDE-PREFIXES is provided (string or a list of strings),
directories starting with these prefixes will be excluded from the
results."
  (when-let* ((repo-dir (+repo-root))
              (prj-list (expand-file-name ".repo/project.list" repo-dir))
              (projs (and (file-readable-p prj-list)
                          (file-regular-p prj-list)
                          (with-temp-buffer (insert-file-contents prj-list) (string-lines (buffer-string) t)))))
    (if exclude-prefixes
        (seq-filter (lambda (path)
                      (not (seq-some (lambda (prefix) (string-prefix-p prefix path)) exclude-prefixes)))
                    projs)
      projs)))

;;;###autoload
(defun +repo-project-p (&optional proj)
  "Retrun non-nil when PROJ (or the current project) is a Repo project.

When in a Repo project, return the project path relative to the Repo
root."
  (when-let* ((repo-root (+repo-root))
              (projs (mapcar #'file-name-as-directory (+repo-projects)))
              (proj-path (+project-safe-root)))
    (car (member (file-name-as-directory (file-relative-name (+project-safe-root) repo-root)) projs))))


;;;###autoload
(+def-project-mode! +repo-project-mode
  "A minor mode enabled in files/buffers opened in a Repo workspace."
  :when (+repo-project-p)
  :fileless-buffers t)


(provide 'me-repo-x)
;;; me-repo-x.el ends here
