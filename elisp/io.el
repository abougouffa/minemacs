;; -*- lexical-binding: t; -*-

;;;###autoload
(defun +file-mime-type (file)
  "Get MIME type for FILE based on magic codes provided by the 'file' command.
Return a symbol of the MIME type, ex: `text/x-lisp', `text/plain',
`application/x-object', `application/octet-stream', etc."
  (if (executable-find "file")
      (let ((mime-type (shell-command-to-string (format "file --brief --mime-type %s" file))))
        (intern (string-trim-right mime-type)))
    (error "The \"file\" tool isn't installed.")))

;;;###autoload
(defun +file-name-incremental (filename)
  "Return an unique file name for FILENAME.
If \"file.ext\" exists, returns \"file-0.ext\"."
  (let* ((ext (file-name-extension filename))
         (dir (file-name-directory filename))
         (file (file-name-base filename))
         (filename-regex (concat "^" file "\\(?:-\\(?1:[[:digit:]]+\\)\\)?" (if ext (concat "\\." ext) "")))
         (last-file (car (last (directory-files dir nil filename-regex))))
         (last-file-num (when (and last-file (string-match filename-regex last-file) (match-string 1 last-file))))
         (num (1+ (string-to-number (or last-file-num "-1"))))
         (filename (file-name-concat dir (format "%s%s%s" file (if last-file (format "-%d" num) "") (if ext (concat "." ext) "")))))
    filename))

;;;###autoload
(defun +file-read-to-string (filename)
  "Return a string with the contents of FILENAME."
  (when (and (file-exists-p filename) (not (file-directory-p filename)))
    (with-temp-buffer
      (insert-file-contents filename)
      (buffer-string))))

;;;###autoload
(defun +file-directories (dir)
  (when dir
    (seq-filter #'file-directory-p
                (mapcar #'abbreviate-file-name
                        (directory-files dir t)))))

;;;###autoload
(defun +delete-this-file (&optional path force-p)
  "Delete PATH.

If PATH is not specified, default to the current buffer's file.

If FORCE-P, delete without confirmation."
  (interactive
   (list (buffer-file-name (buffer-base-buffer))
         current-prefix-arg))
  (let* ((path (or path (buffer-file-name (buffer-base-buffer))))
         (short-path (abbreviate-file-name path)))
    (unless (and path (file-exists-p path))
      (user-error "Buffer is not visiting any file"))
    (unless (file-exists-p path)
      (error "File doesn't exist: %s" path))
    (unless (or force-p (y-or-n-p (format "Really delete %S?" short-path)))
      (user-error "Aborted"))
    (let ((buf (current-buffer)))
      (unwind-protect
          (progn (delete-file path t) t)
        (when (file-exists-p path)
          (error "Failed to delete %S" short-path))))))

;;;###autoload
(defun +move-this-file (new-path &optional force-p)
  "Move current buffer's file to NEW-PATH.

If FORCE-P, overwrite the destination file if it exists, without confirmation."
  (interactive
   (list (read-file-name "Move file to: ")
         current-prefix-arg))
  (unless (and buffer-file-name (file-exists-p buffer-file-name))
    (user-error "Buffer is not visiting any file"))
  (let ((old-path (buffer-file-name (buffer-base-buffer)))
        (new-path (expand-file-name new-path)))
    (when (directory-name-p new-path)
      (setq new-path (expand-file-name (file-name-nondirectory old-path) new-path)))
    (make-directory (file-name-directory new-path) t)
    (rename-file old-path new-path (or force-p 1))
    (set-visited-file-name new-path t t)
    ;; (doom-files--update-refs old-path new-path)
    (message "File moved to %S" (abbreviate-file-name new-path))))

(defun +sudo-file-path (file)
  (let ((host (or (file-remote-p file 'host) "localhost")))
    (concat "/" (when (file-remote-p file)
                  (concat (file-remote-p file 'method) ":"
                          (if-let (user (file-remote-p file 'user))
                              (concat user "@" host)
                            host)
                          "|"))
            "sudo:root@" host
            ":" (or (file-remote-p file 'localname)
                    file))))

;;;###autoload
(defun +sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (find-file (+sudo-file-path file)))

;;;###autoload
(defun +sudo-this-file ()
  "Open the current file as root."
  (interactive)
  (if-let ((this-file (or buffer-file-name
                          (when (or (derived-mode-p 'dired-mode)
                                    (derived-mode-p 'wdired-mode))
                            default-directory))))
      (find-file (+sudo-file-path this-file))
    (user-error "Current buffer not bound to a file")))

;;;###autoload
(defun +sudo-save-buffer ()
  "Save this file as root."
  (interactive)
  (if buffer-file-name
      (if-let ((file (+sudo-file-path buffer-file-name))
               (buffer (find-file-noselect file))
               (origin (current-buffer)))
          (progn
            (copy-to-buffer buffer (point-min) (point-max))
            (unwind-protect
                (with-current-buffer buffer
                  (save-buffer))
              (unless (eq origin buffer)
                (kill-buffer buffer))
              (with-current-buffer origin
                (revert-buffer t t))))
        (user-error "Unable to open %S" file))
    (user-error "Current buffer not bound to a file")))

;;;###autoload
(defun +yank-this-file-name ()
  "Yank the file name of this buffer."
  (interactive)
  (let ((file (buffer-file-name)))
    (if (null file)
        (user-error "This buffer isn't bound to a file.")
      (with-temp-buffer
        (insert file)
        (kill-ring-save (point-min) (point-max))))))

;;;###autoload
(defun +clean-file-name (filename &optional conv-downcase)
  "Clean file name."
  ;; Clean slashes, backslashes, ":", ";", spaces, and tabs
  (replace-regexp-in-string
   "[:;\t /\\_]+" "-"
   (if conv-downcase (downcase filename) filename)))
