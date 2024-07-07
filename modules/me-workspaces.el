;;; workspaces.el --- Windows, workspaces (via tab-bar & tab-line) -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;; IDEA:
;; - github.com/fritzgrabo/project-tab-groups
;; - github.com/florommel/bufferlo
;; - www.rousette.org.uk/archives/using-the-tab-bar-in-emacs

;;; Code:

(use-package project-tab-groups
  :straight t
  :after project
  :init
  (setq project-tab-groups-tab-group-name-function #'+project-tab-groups-name-by-project-root)
  (project-tab-groups-mode 1)
  :config
  (defvar +project-tab-groups-unique-map (make-hash-table :test 'equal))
  (defun +project-tab-groups-name-by-project-root (dir)
    "Use the project root as group name starting from DIR."
    (with-temp-buffer
      (setq default-directory dir)
      (hack-dir-local-variables-non-file-buffer)
      (let ((root (or (when-let* ((proj (project-current)))
                        (expand-file-name (project-root proj))) ;; Use the full path
                      (expand-file-name dir))))
        root)))
  ;; Rename the tab to the tab-group name (project name)
  (advice-add
   #'project-tab-groups--select-or-create-tab-group :after-while
   (satch-defun +project-tab-groups--name-tab-by-group:after-while-a (&rest _)
     (when-let ((group-path (alist-get 'group (tab-bar--current-tab))))
       (+unique-name-register group-path :map '+project-tab-groups-unique-map)
       ;; Rename all tabs accordingly
       (dolist (frame (frame-list))
         (dolist (tab (frame-parameter frame 'tabs))
           (when-let* ((group-path (alist-get 'group tab))
                       (unique (gethash group-path +project-tab-groups-unique-map)))
             (setcdr (assoc 'name tab) (alist-get 'unique-name unique))
             (setcdr (assoc 'explicit-name tab) t)))))
     t)))

(use-package burly
  :straight t)

(use-package bufler
  :straight t)


(provide 'me-workspaces)

;;; me-workspaces.el ends here
