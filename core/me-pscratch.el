;;; me-pscratch.el --- Persistent and per-project scratch buffers -*- lexical-binding: t; -*-

;; -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:
;; This code has been adapted and simplified from Doom Emacs.

;;; Code:

(defvar +scratch-default-file "__default"
  "The default file name for a project-less scratch buffer.

Will be saved in `+scratch-dir'.")

(defvar +scratch-dir (concat minemacs-local-dir "pscratch/")
  "Where to save persistent scratch buffers.")

(defvar +scratch-initial-major-mode nil
  "What major mode to start fresh scratch buffers in.

Scratch buffers preserve their last major mode, however, so this only affects
the first, fresh scratch buffer you create. This accepts:

  t           Inherits the major mode of the last buffer you had selected.
  nil         Uses `fundamental-mode'
  MAJOR-MODE  Any major mode symbol")

(defvar +scratch-buffers nil
  "A list of active scratch buffers.")

(defvar +scratch-current-project nil
  "The name of the project associated with the current scratch buffer.")
(put '+scratch-current-project 'permanent-local t)

(defvar +scratch-buffer-hook ()
  "The hooks to run after a scratch buffer is created.")

;;;###autoload
(defun +scratch-load-persistent-scratch-buffer (&optional project-name)
  (setq-local +scratch-current-project (or project-name +scratch-default-file))
  (let ((smart-scratch-file
         (expand-file-name (concat +scratch-current-project ".el") +scratch-dir)))
    (make-directory +scratch-dir t)
    (when (file-readable-p smart-scratch-file)
      (+log! "Reading persistent scratch from %s" smart-scratch-file)
      (cl-destructuring-bind (content point mode)
          (with-temp-buffer
            (save-excursion (insert-file-contents smart-scratch-file))
            (read (current-buffer)))
        (erase-buffer)
        (funcall mode)
        (insert content)
        (goto-char point)
        t))))

;;;###autoload
(defun +scratch-buffer (&optional dont-restore-p mode directory project-name)
  "Return a scratchpad buffer in major MODE."
  (let* ((buffer-name (if project-name (format "*pscratch:%s*" project-name) "*pscratch*"))
         (buffer (get-buffer buffer-name)))
    (with-current-buffer
        (or buffer (get-buffer-create buffer-name))
      (setq default-directory directory)
      (setq-local so-long--inhibited t)
      (if dont-restore-p
          (erase-buffer)
        (unless buffer
          (+scratch-load-persistent-scratch-buffer project-name)
          (when (and (eq major-mode 'fundamental-mode)
                     (functionp mode))
            (funcall mode))))
      (cl-pushnew (current-buffer) +scratch-buffers)
      (+hook-once! 'window-buffer-change-functions (+scratch-persist-buffers-h))
      (+hook-once! 'server-visit-hook (+scratch-persist-buffers-h))
      (+hook-once! 'window-selection-change-functions (+scratch-persist-buffers-h))
      (add-hook 'kill-buffer-hook #'+scratch-persist-buffer-h nil 'local)
      (run-hooks '+scratch-buffer-created-hook)
      (current-buffer))))


;;; Persistent scratch buffer

;;;###autoload
(defun +scratch-persist-buffer-h (&rest _)
  "Save the current buffer to `+scratch-dir'."
  (let ((content (buffer-substring-no-properties (point-min) (point-max)))
        (point (point))
        (mode major-mode))
    (with-temp-file (expand-file-name (concat (or +scratch-current-project
                                                  +scratch-default-file)
                                              ".el")
                                      +scratch-dir)
      (prin1 (list content point mode)
             (current-buffer)))))

;;;###autoload
(defun +scratch-persist-buffers-h (&rest _)
  "Save all scratch buffers to `+scratch-dir'."
  (setq +scratch-buffers
        (cl-delete-if-not #'buffer-live-p +scratch-buffers))
  (dolist (buffer +scratch-buffers)
    (with-current-buffer buffer
      (+scratch-persist-buffer-h))))

;;;###autoload
(defun +scratch-persist-buffers-after-switch-h (&rest _)
  "Kill scratch buffers when they are no longer visible, saving them to disk."
  (unless (cl-some #'get-buffer-window +scratch-buffers)
    (mapc #'kill-buffer +scratch-buffers)
    (remove-hook '+switch-buffer-hook #'+scratch-persist-buffers-after-switch-h)))

;;;###autoload
(unless noninteractive
  (add-hook 'kill-emacs-hook #'+scratch-persist-buffers-h))


;;; Commands

;;;###autoload
(defun +scratch-open-buffer (&optional arg project-p same-window-p)
  "Pop up a persistent scratch buffer.

If passed the prefix ARG, do not restore the last scratch buffer.
If PROJECT-P is non-nil, open a persistent scratch buffer associated with the
  current project."
  (interactive "P")
  (funcall
   (if same-window-p
       #'switch-to-buffer
     #'pop-to-buffer)
   (+scratch-buffer
    arg
    (cond
     ((eq +scratch-initial-major-mode t)
      (unless (or buffer-read-only
                  (derived-mode-p 'special-mode)
                  (string-match-p "^ ?\\*" (buffer-name)))
        major-mode)
      ((symbolp +scratch-initial-major-mode)
       +scratch-initial-major-mode)
      ((null +scratch-initial-major-mode)
       nil)))
    default-directory
    (when-let ((project (project-current))
               project-p)
      (project-name project)))))

;;;###autoload
(defun +switch-to-scratch-buffer (&optional arg project-p)
  "Like `+scratch-open-buffer', but switches to it in the current window.

If passed the prefix ARG, do not restore the last scratch buffer."
  (interactive "P")
  (+scratch-open-buffer arg project-p 'same-window))

;;;###autoload
(defun +scratch-open-project-scratch-buffer (&optional arg same-window-p)
  "Opens the (persistent) project scratch buffer in a popup.

If passed the prefix ARG, do not restore the last scratch buffer."
  (interactive "P")
  (+scratch-open-buffer arg 'project same-window-p))

;;;###autoload
(defun +scratch-switch-to-project-scratch-buffer (&optional arg)
  "Like `+scratch-open-project-scratch-buffer', but switches to it in the current
window.

If passed the prefix ARG, do not restore the last scratch buffer."
  (interactive "P")
  (+scratch-open-project-scratch-buffer arg 'same-window))

;;;###autoload
(defun +scratch-revert-scratch-buffer ()
  "Revert scratch buffer to last persistent state."
  (interactive)
  (unless (string-match-p "^\\*pscratch" (buffer-name))
    (user-error "Not in a scratch buffer"))
  (when (+scratch-load-persistent-scratch-buffer +scratch-current-project)
    (message "Reloaded scratch buffer")))

;;;###autoload
(defun +scratch-delete-persistent-scratch-file (&optional arg)
  "Deletes a scratch buffer file in `+scratch-dir'.

If prefix ARG, delete all persistent scratches."
  (interactive)
  (if arg
      (progn
        (delete-directory +scratch-dir t)
        (message "Cleared %S" (abbreviate-file-name +scratch-dir)))
    (make-directory +scratch-dir t)
    (let ((file (read-file-name "Delete scratch file > " +scratch-dir "scratch")))
      (if (not (file-exists-p file))
          (message "%S does not exist" (abbreviate-file-name file))
        (delete-file file)
        (message "Successfully deleted %S" (abbreviate-file-name file))))))


(provide 'me-pscratch)

;;; me-pscratch.el ends here
