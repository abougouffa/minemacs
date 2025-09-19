;;; me-vc-x.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@cneebg.pbz")
;; Created: 2025-09-04
;; Last modified: 2025-09-04

;;; Commentary:

;;; Code:

;;;###autoload(with-eval-after-load 'vc-git (require 'me-vc-x))

(require 'vc-git)

;;;###autoload(keymap-global-set "C-x C-g" '+switch-git-status-buffer)

;; Based on https://www.rahuljuliato.com/posts/switch-git-status-buffer
;;;###autoload
(defun +switch-git-status-buffer ()
  "Parse git status from an expanded path and switch to a file.
The completion candidates include the Git status of each file."
  (interactive)
  (if-let* ((repo-root (vc-git-root default-directory)))
      (let* ((expanded-root (expand-file-name repo-root))
             (command-to-run (format "git -C %s status --porcelain=v1" (shell-quote-argument expanded-root)))
             (cmd-output (shell-command-to-string command-to-run))
             (target-files
              (let (files)
                (dolist (line (split-string cmd-output "\n" t) (nreverse files))
                  (when (> (length line) 3)
                    (let ((status (substring line 0 2))
                          (path-info (substring line 3)))
                      ;; Handle rename specially
                      (if (string-match "^R" status)
                          (when-let* ((paths (split-string path-info " -> " t))
                                      (new-path (cadr paths)))
                            (push (cons (format "R %s" new-path) new-path) files))
                        ;; Modified or untracked
                        (when (or (string-match "M" status) (string-match "\?\?" status))
                          (push (cons (format "%s %s" status path-info) path-info) files)))))))))
        (if (not target-files)
            (message "No modified or renamed files found.")
          (when-let* ((selection (completing-read "Switch to buffer (Git modified): " (mapcar #'car target-files) nil t))
                      (file-path (cdr (assoc selection target-files))))
            (find-file (expand-file-name file-path expanded-root)))))
    (user-error "Not inside a Git repository")))


(provide 'me-vc-x)
;;; me-vc-x.el ends here
