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
  (defun +project-tab-groups-name-by-project-root (dir)
    "Derive tab group name for project in DIR."
    (with-temp-buffer
      (setq default-directory dir)
      (hack-dir-local-variables-non-file-buffer)
      (let ((name (or (and (boundp 'tab-group-name) tab-group-name)
                      (and (boundp 'project-name) project-name)
                      (and (fboundp 'project-name)
                           (when-let ((proj (project-current)))
                             (format "%s (%s)" (project-name proj) (substring (sha1 (project-root proj)) nil 8))))
                      (file-name-nondirectory (directory-file-name dir))))
            (name-template (or (and (boundp 'tab-group-name-template) tab-group-name-template)
                               (and (boundp 'project-name-template) project-name-template)
                               "%s")))
        (format name-template name)))))


(provide 'me-workspaces)

;;; me-workspaces.el ends here
