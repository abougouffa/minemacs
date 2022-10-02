;; -*- lexical-binding: t; -*-

(with-eval-after-load 'project
  (custom-variable-set 'project-list-file
                       (expand-file-name "projects" minemacs-var-dir)))
