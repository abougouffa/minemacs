;;; inotify-revert.el --- Auto-revert using inotify -*- lexical-binding: t; -*-

;; Copyright (c) 2019 fmdkdd
;; Copyright 2024 Abdelhak BOUGOUFFA

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Like auto-revert-mode, but without the weird lag.
;;
;; auto-revert-mode supposedly uses inotify thourgh filenotify, and reverts to
;; polling when inotify fails.  On my machine, auto-revert can take up to 1sec
;; to revert a file that has changed, while using this mode is instantaneous.

;; From: https://github.com/fmdkdd/dotfiles/blob/master/emacs/.emacs.d/elisp/inotify-revert.el

;;; Code:

(require 'filenotify)

(defvar-local inotify-revert--descriptor nil
  "The filenotify descriptor for the current buffer.")

(defun inotify-revert-activate ()
  "Start watching the current buffer's file for changes."
  (when buffer-file-name
    (let ((buffer (current-buffer)))
      (setq inotify-revert--descriptor
            (file-notify-add-watch
             buffer-file-name '(change)
             ;; Using a lambda to capture the buffer, instead of a handler in a
             ;; separate defun
             (lambda (event)
               (when (eq (nth 1 event) 'changed)
                 (with-current-buffer buffer
                   (revert-buffer 'ignore-auto 'noconfirm)))))))))

(defun inotify-revert-deactivate ()
  "Unsubscribe to the current buffer's file changes."
  (when inotify-revert--descriptor
    (file-notify-rm-watch inotify-revert--descriptor)
    (kill-local-variable inotify-revert--descriptor)))

;;;###autoload
(define-minor-mode inotify-revert-mode
  "Like `auto-revert-mode', but faster."
  :lighter " revert"
  (cond
   (inotify-revert-mode
    (inotify-revert-activate))
   ((not inotify-revert-mode)
    (inotify-revert-deactivate))))

(provide 'inotify-revert)

;;; inotify-revert.el ends here
