;; -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


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
(defun +directory-subdirs (dir)
  "Return a list of sub-directories in DIR."
  (when dir
    (seq-filter #'file-directory-p
                (mapcar #'abbreviate-file-name
                        (directory-files dir t "[^.][^.]?$")))))

;;;###autoload
(defun +directory-ensure (path)
  "Ensure PATH exists, if not create it, return PATH."
  (let ((parent-dir (file-name-directory path)))
    (unless (file-directory-p parent-dir)
      (ignore-errors (mkdir parent-dir t))
      (unless (file-directory-p parent-dir)
        (+error! "Cannot create directory %s" parent-dir))))
  path)

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

;;;###autoload
(defun +tramp-sudo-file-path (file)
  "Construct a Tramp sudo path to FILE. Works for both local and remote files."
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
  (find-file (+tramp-sudo-file-path file)))

;;;###autoload
(defun +sudo-this-file ()
  "Open the current file as root."
  (interactive)
  (if-let ((this-file (or buffer-file-name
                          (when (or (derived-mode-p 'dired-mode)
                                    (derived-mode-p 'wdired-mode))
                            default-directory))))
      (find-file (+tramp-sudo-file-path this-file))
    (user-error "Current buffer not bound to a file")))

;;;###autoload
(defun +sudo-save-buffer ()
  "Save this file as root."
  (interactive)
  (if buffer-file-name
      (if-let ((file (+tramp-sudo-file-path buffer-file-name))
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
(defun +clean-file-name (filename &optional downcase-p)
  "Clean file name."
  ;; Clean slashes, backslashes, ":", ";", spaces, and tabs
  (replace-regexp-in-string
   "[:;\t\n\r /\\_]+" "-"
   (replace-regexp-in-string
    "[‘’‚’“”„”\"`'()]+" ""
    (if downcase-p (downcase filename) filename))))

;;;###autoload
(defun +html2pdf (infile outfile)
  "Convert HTML file INFILE to PDF and save it to OUTFILE."
  (call-process "wkhtmltopdf" nil nil nil
                "--images" "--disable-javascript" "--enable-local-file-access"
                "--encoding" "utf-8"
                infile outfile))

;;;###autoload
(defun +txt2html (infile outfile &optional mail-mode-p)
  "Convert plain-text file INFILE to HTML and save it to OUTFILE.
When MAIL-MODE-P is non-nil, --mailmode is passed to \"txt2html\"."
  (apply
   #'call-process
   (append '("txt2html" nil nil nil "-8")
           (when mail-mode-p '("--mailmode"))
           (list "--outfile" outfile infile))))

(defvar +save-as-pdf-filename nil
  "File name to use, if non-nil, for the output file.")

;;;###autoload
(defun +save-as-pdf (infile &optional mail-mode-p)
  "Save URL as PDF.
This function's signature is compatible with `browse-url-browser-function'
so it can be used to save HTML pages or emails to PDF.
When MAIL-MODE-P is non-nil, treat INFILE as a mail."
  (let ((outfile (or +save-as-pdf-filename
                     (expand-file-name
                      (file-name-with-extension (file-name-base infile) ".pdf")
                      (file-name-directory infile)))))
    (if (zerop
         ;; For HTML files, just call `+html2pdf'
         (if (string= "html" (file-name-extension infile))
             (+html2pdf infile outfile)
           ;; For non-HTML (plain-text) files, convert them to HTML then call `+html2pdf'
           (let ((tmp-html (make-temp-file "txt2html-" nil ".html")))
             (+txt2html infile tmp-html mail-mode-p)
             (+html2pdf tmp-html outfile))))
        (message "Exported PDF to %s"
                 (truncate-string-to-width
                  (abbreviate-file-name outfile)
                  (/ (window-width (minibuffer-window)) 2) nil nil t))
      (user-error
       (if (file-exists-p outfile)
           "PDF created but with some errors!"
         "An error occured, cannot create the PDF!")))))
