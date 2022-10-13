;; -*- lexical-binding: t; -*-

(defvar +mu4e-account-aliases nil
  "Per-account alias list.")

(defvar +mu4e-auto-bcc-address nil
  "BCC address.")

;; Some of these functions are adapted from Doom Emacs

(defun +mu4e-view-select-attachment ()
  "Use completing-read to select a single attachment.
Acts like a singular `mu4e-view-save-attachments', without the saving."
  (if-let ((parts (delq nil (mapcar
                             (lambda (part)
                               (when (assoc "attachment" (cdr part))
                                 part))
                             (mu4e~view-gather-mime-parts))))
           (files (+mu4e-part-selectors parts)))
      (cdr (assoc (completing-read "Select attachment: " (mapcar #'car files)) files))
    (user-error (mu4e-format "No attached files found"))))


(defun +mu4e-view-open-attachment ()
  "Select an attachment, and open it."
  (interactive)
  (mu4e~view-open-file
   (mu4e~view-mime-part-to-temp-file (cdr (+mu4e-view-select-attachment)))))


(defun +mu4e-view-select-mime-part-action ()
  "Select a MIME part, and perform an action on it."
  (interactive)
  (let ((labeledparts (+mu4e-part-selectors (mu4e~view-gather-mime-parts))))
    (if labeledparts
        (mu4e-view-mime-part-action
         (cadr (assoc (completing-read "Select part: " (mapcar #'car labeledparts))
                      labeledparts)))
      (user-error (mu4e-format "No parts found")))))


(defun +mu4e-part-selectors (parts)
  "Generate selection strings for PARTS."
  (if parts
      (let (partinfo labeledparts maxfnamelen fnamefmt maxsizelen sizefmt)
        (dolist (part parts)
          (push (list :index (car part)
                      :mimetype (if (and (string= "text/plain" (caaddr part))
                                         (alist-get 'charset (cdaddr part)))
                                    (format "%s (%s)"
                                            (caaddr part)
                                            (alist-get 'charset (cdaddr part)))
                                  (caaddr part))
                      :type (car (nth 5 part))
                      :filename (cdr (assoc 'filename (assoc "attachment" (cdr part))))
                      :size (file-size-human-readable (with-current-buffer (cadr part) (buffer-size)))
                      :part part)
                partinfo))
        (setq maxfnamelen (apply #'max 7 (mapcar (lambda (i) (length (plist-get i :filename))) partinfo))
              fnamefmt (format " %%-%ds  " maxfnamelen)
              maxsizelen (apply #'max (mapcar (lambda (i) (length (plist-get i :size))) partinfo))
              sizefmt (format "%%-%ds " maxsizelen))
        (dolist (pinfo partinfo)
          (push (cons (concat (propertize (format "%-2s " (plist-get pinfo :index)) 'face '(bold font-lock-type-face))
                              (when (featurep 'all-the-icons)
                                (all-the-icons-icon-for-file (or (plist-get pinfo :filename) "")))
                              (format fnamefmt (or (plist-get pinfo :filename)
                                                   (propertize (plist-get pinfo :type) 'face '(italic font-lock-doc-face))))
                              (format sizefmt (propertize (plist-get pinfo :size) 'face 'font-lock-builtin-face))
                              (propertize (plist-get pinfo :mimetype) 'face 'font-lock-constant-face))
                      (plist-get pinfo :part))
                labeledparts))
        labeledparts)))


(defun +mu4e-view-save-all-attachments (&optional arg)
  "Save all MIME parts from current mu4e gnus view buffer."
  ;; Copied from mu4e-view-save-attachments
  (interactive "P")
  (cl-assert (and (eq major-mode 'mu4e-view-mode)
                  (derived-mode-p 'gnus-article-mode)))
  (let* ((msg (mu4e-message-at-point))
         (id (me-clean-file-name (mu4e-message-field msg :subject) :downcase))
         (attachdir (expand-file-name id mu4e-attachment-dir))
         (parts (mu4e~view-gather-mime-parts))
         (handles '())
         (files '())
         dir)
    (mkdir attachdir t)
    (dolist (part parts)
      (let ((fname (or (cdr (assoc 'filename (assoc "attachment" (cdr part))))
                       (seq-find #'stringp
                                 (mapcar (lambda (item) (cdr (assoc 'name item)))
                                         (seq-filter 'listp (cdr part)))))))
        (when fname
          (push `(,fname . ,(cdr part)) handles)
          (push fname files))))
    (if files
        (progn
          (setq dir
                (if arg (read-directory-name "Save to directory: ")
                  attachdir))
          (cl-loop for (f . h) in handles
                   when (member f files)
                   do (mm-save-part-to-file h
                                            (me-file-name-incremental
                                             (expand-file-name f dir)))))
      (mu4e-message "No attached files found"))))


(defun +mu4e-register-account (label maildir letvars &optional default)
  (let ((context
         (make-mu4e-context
          :name label
          :enter-func
          (lambda () (mu4e-message "Switched to %s" label))
          :leave-func
          (lambda ()
            (setq +mu4e-account-aliases nil)
            (mu4e-clear-caches))
          :match-func
          (lambda (msg)
            (when msg
              (string-match-p (concat "[/]?" maildir) (mu4e-message-field msg :maildir))))
          :vars letvars)))
    (add-to-list 'mu4e-contexts context default)
    context))


;; I like to always BCC myself
(defun +mu4e--auto-bcc-h ()
  "Add BCC address from `+mu4e-auto-bcc-address'."
  (when +mu4e-auto-bcc-address
    (save-excursion (message-add-header (format "BCC: %s\n" +mu4e-auto-bcc-address)))))


(defun +mu4e--set-from-address-h ()
  "If the user defines multiple `+mu4e-account-aliases' for email aliases
within a context, set `user-mail-address' to an alias found in the 'To' or
'From' headers of the parent message if present, or prompt the user for a
preferred alias"
  (when-let ((addresses (if (or mu4e-contexts +mu4e-account-aliases)
                            (cons user-mail-address ;; the main address
                                  +mu4e-account-aliases) ;; the aliases
                          (mu4e-personal-addresses))))
    (setq user-mail-address
          (if mu4e-compose-parent-message
              (let ((to (mapcar (lambda (a) (plist-get a :email))
                                (mu4e-message-field mu4e-compose-parent-message :to)))
                    (from (mapcar (lambda (a) (plist-get a :email))
                                  (mu4e-message-field mu4e-compose-parent-message :from))))
                (or (car (seq-intersection to addresses))
                    (car (seq-intersection from addresses))
                    (completing-read "From: " addresses)))
            (completing-read "From: " addresses)))))


;; Detect empty subjects, and give users an opotunity to fill something in
(defun +mu4e--check-for-subject-h ()
  "Check that a subject is present, and prompt for a subject if not."
  (save-excursion
    (goto-char (point-min))
    (search-forward "--text follows this line--")
    (re-search-backward "^Subject:") ; this should be present no matter what
    (let ((subject (string-trim (substring (thing-at-point 'line) 8))))
      (when (string-empty-p subject)
        (end-of-line)
        (insert (read-string "Subject (optional): "))
        (message "Sending...")))))


(defun +mu4e-save-message-at-point (&optional dir)
  "Copy message at point to somewhere else as <date>_<subject>.eml."
  (interactive)
  (let* ((msg (mu4e-message-at-point))
         (target (format "%s_%s.eml"
                         (format-time-string "%F" (mu4e-message-field msg :date))
                         (me-clean-file-name (or (mu4e-message-field msg :subject) "No subject") :downcase))))
    (copy-file
     (mu4e-message-field msg :path)
     (format "%s/%s" (or dir mu4e-attachment-dir (read-directory-name "Copy message to: ")) target) 1)))


;;;###autoload
(defun me-mu4e-extras-setup ()
  (add-hook 'mu4e-compose-mode-hook '+mu4e--auto-bcc-h)
  (add-hook 'mu4e-compose-pre-hook '+mu4e--set-from-address-h)
  (add-hook 'message-send-hook #'+mu4e--check-for-subject-h)

  ;; Setup keybindings
  (me-map-key :keymaps 'mu4e-view-mode-map
    "p" #'mu4e-view-save-attachments
    "P" #'+mu4e-view-save-all-attachments
    "A" #'+mu4e-view-select-mime-part-action
    "o" #'+mu4e-view-open-attachment
    "O" #'+mu4e-save-message-at-point))


(provide 'me-mu4e-extras)
