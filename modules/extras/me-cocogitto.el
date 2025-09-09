;;; me-mu4e-ui.el --- Better UI for mu4e -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2023-10-08
;; Last modified: 2025-09-09

;;; Commentary:

;;; Code:

(defvar +cocogitto-buffer-name " *cocogitto*")

;;;###autoload
(progn
  (defvar +cocogitto-program "cog")
  (defvar +cocogitto-available-p nil)
  (defun +cocogitto-available-p (&rest _args)
    (with-memoization +cocogitto-available-p
      (and (executable-find +cocogitto-program) t))))

;;;###autoload
(defun +cocogitto-bump (level &optional pre)
  "Bump version LEVEL (`auto', `major', `minor' or `patch').

When PRE is provided, it is used as pre-release suffix.

Call with \\[universal-argument] for applying an `auto' bump.

This command stashes the current workspace before bumping the version, and
restores it after that."
  (interactive
   (if current-prefix-arg
       '("auto" nil)
     (list
      (if (yes-or-no-p "Manually set the target version? ")
          (concat "version " (read-string "Version: "))
        (completing-read "Increment the version: " '(auto major minor patch)
                         (when (yes-or-no-p "Is this version a pre-release? ")
                           (read-string "Pre-release version: ")))))))
  (if-let* ((root-dir (+project-safe-root)))
      (with-current-buffer (get-buffer-create +cocogitto-buffer-name)
        (setq default-directory root-dir)
        (conf-colon-mode)
        (insert (format "############ Cocogitto bump (%s) ############\n" level))
        (let ((stashed (string-match-p "COCOGITTO SAVED STATE" (shell-command-to-string "git stash -u -m \"COCOGITTO SAVED STATE\""))))
          (call-process-shell-command (format "%s bump --%s%s" +cocogitto-program level (if pre (format " --pre %s" pre) "")) nil (current-buffer))
          (when stashed
            (call-process-shell-command "git stash pop" nil (current-buffer))))
        (message "Cocogitto finished!"))
    (user-error "Not in a Git managed directory")))

;;;###autoload
(put '+cocogitto-bump 'completion-predicate (lambda (_cmd _buf) (+cocogitto-available-p)))


(provide 'me-cocogitto)

;;; me-cocogitto.el ends here
