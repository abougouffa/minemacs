;;; me-mu4e-ui.el --- Better UI for mu4e -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(defvar +cocogitto-buffer-name " *cocogitto*")

;;;###autoload
(defun +cocogitto-bump (level &optional pre)
  "Bump version LEVEL (`auto', `major', `minor' or `patch'), and with PRE if it
is a pre-release.

This command stashes the current workspace before bumping the version, and
restores it after that."
  (interactive
   (list (if (yes-or-no-p "Manually set the target version? ")
             (concat "version " (read-string "Vesion: "))
           (completing-read "Increment the version: " '(auto major minor patch)))
         (when (yes-or-no-p "Is this version a pre-release? ")
           (read-string "Pre-release version: "))))
  (with-current-buffer (get-buffer-create +cocogitto-buffer-name)
    (conf-colon-mode)
    (insert (format "############ Cocogitto bump (%s) ############\n" level))
    (call-process-shell-command "git stash -u" nil (current-buffer))
    (call-process-shell-command
     (format "cog bump --%s%s" level (if pre (format "--pre %s" pre) ""))
     nil (current-buffer))
    (call-process-shell-command "git stash pop" nil (current-buffer))))


(provide 'me-cocogitto)

;;; me-cocogitto.el ends here
