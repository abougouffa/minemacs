;; -*- lexical-binding: t; -*-

;;;###autoload
(defun me-dir-locals-reload-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

;;;###autoload
(defun me-dir-locals-reload-for-all-buffers-in-this-directory ()
  "For every buffer with the same `default-directory` as the
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir)
          (me-dir-locals-reload-for-current-buffer))))))

;;;###autoload
(defun me-dir-locals-enable-autoreload ()
  (when (and (buffer-file-name)
             (equal dir-locals-file (file-name-nondirectory (buffer-file-name))))
    (message "Dir-locals will be reloaded after saving.")
    (add-hook 'after-save-hook 'me-dir-locals-reload-for-all-buffers-in-this-directory nil t)))

;;;###autoload
(defun me-dir-locals-open-or-create ()
  "Open or create the dir-locals.el for the current project."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (base-dir (car (ensure-list (dir-locals-find-file file-name)))))
    (find-file
     (cond (base-dir (expand-file-name dir-locals-file base-dir))
           ((project-current) (expand-file-name dir-locals-file (project-root (project-current))))
           ((vc-root-dir) (expand-file-name dir-locals-file (vc-root-dir)))
           (t (expand-file-name dir-locals-file (file-name-directory file-name)))))))
