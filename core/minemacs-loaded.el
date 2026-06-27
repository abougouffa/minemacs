;; minemacs-loaded.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-10-02
;; Last modified: 2026-06-27

;;; Commentary:

;;; Code:

(defun +minemacs-run-hook-wrapper (func &optional prefix)
  "Wrapper to run FUNC in `run-hook-wrapped', use PREFIX for logs."
  (let ((prefix (or prefix "RunHook")))
    (condition-case err
        (funcall func)
      (error (+msg! (concat prefix "Error") "%S" err))
      (:success (when (or minemacs-debug-p minemacs-verbose-p) (+msg! (concat prefix "Success") "Executed %S" func))))
    ;; Return nil to continue running hooks, otherwise, `run-hook-wrapped' will stop running hooks
    nil))

;; Run hooks
(+log! "Running %d `minemacs-after-startup-hook' hooks." (length minemacs-after-startup-hook))
(run-hook-wrapped 'minemacs-after-startup-hook #'+minemacs-run-hook-wrapper "AfterStartupLoad")

(+log! "Providing `minemacs-loaded'.")
(provide 'minemacs-loaded)

(let* ((ver-file (expand-file-name "last-emacs-version" minemacs-cache-dir))
       (curr-ver (format "%s-%d-%s" emacs-version emacs-build-number (substring emacs-repository-version 0 8)))
       (prev-ver (and (file-exists-p ver-file)
                      (with-temp-buffer
                        (insert-file-contents ver-file)
                        (string-trim (buffer-substring-no-properties (point-min) (point-max)))))))
  (unless (equal curr-ver prev-ver)
    (message "Detected Emacs version change from %s to %s" prev-ver curr-ver)
    (run-hook-wrapped 'minemacs-after-emacs-version-change-hook #'+minemacs-run-hook-wrapper "AfterEmacsVerChange")
    (with-temp-buffer
      (insert curr-ver)
      (+shutup! (write-file ver-file)))))

(if minemacs-not-lazy-p
    (progn ; If `minemacs-not-lazy-p' is true, force loading lazy hooks immediately
      (+log! "Loading %d lazy packages immediately." (length minemacs-lazy-hook))
      (run-hook-wrapped 'minemacs-lazy-hook #'+minemacs-run-hook-wrapper "ImmediateLoad")
      (provide 'minemacs-lazy))
  (+log! "loading %d lazy packages incrementally." (length minemacs-lazy-hook))
  (cl-callf2 append minemacs-lazy-hook minemacs--lazy-functions
    '((lambda () (provide 'minemacs-lazy))))) ;; Provide `minemacs-lazy' at the end

(defvar minemacs--lazy-timer
  (run-with-timer
   0.1 0.001
   (lambda ()
     (if minemacs--lazy-functions
         (+minemacs-run-hook-wrapper (pop minemacs--lazy-functions) "DeferredLoad")
       (cancel-timer minemacs--lazy-timer)))))


;;; minemacs-loaded.el ends here
