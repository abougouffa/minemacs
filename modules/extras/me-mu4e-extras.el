;;; me-mu4e-extras.el --- Extra tweaks and utilities for mu4e -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(require 'mu4e)
(require 'me-mu4e-gmail)

(autoload 'nerd-icons-icon-for-file "nerd-icons")

(defcustom +mu4e-account-aliases nil
  "Per-account alias list."
  :group 'minemacs-mu4e
  :type '(repeat string))

(defcustom +mu4e-auto-bcc-address nil
  "Auto add this/these addresses as BCC."
  :group 'minemacs-mu4e
  :type '(choice string (repeat string)))

;; Some of these functions are adapted from Doom Emacs

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
          (push (cons
                 (concat
                  (propertize (format "%-2s " (plist-get pinfo :index)) 'face '(bold font-lock-type-face))
                  (nerd-icons-icon-for-file (or (plist-get pinfo :filename) ""))
                  (format fnamefmt (or (plist-get pinfo :filename)
                                       (propertize (plist-get pinfo :type) 'face '(italic font-lock-doc-face))))
                  (format sizefmt (propertize (plist-get pinfo :size) 'face 'font-lock-builtin-face))
                  (propertize (plist-get pinfo :mimetype) 'face 'font-lock-constant-face))
                 (plist-get pinfo :part))
                labeledparts))
        labeledparts)))

(defun +mu4e-view-select-attachment ()
  "Use `completing-read' to select a single attachment.
Acts like a singular `mu4e-view-save-attachments', without the saving."
  (if-let* ((parts (delq nil (mapcar
                              (lambda (part)
                                (when (assoc "attachment" (cdr part))
                                  part))
                              (mu4e-view-mime-parts))))
            (files (+mu4e-part-selectors parts)))
      (cdr (assoc (completing-read "Select attachment: " (mapcar #'car files)) files))
    (user-error (mu4e-format "No attached files found"))))

(defun +mu4e-view-open-attachment ()
  "Select an attachment, and open it."
  (interactive)
  (mu4e--view-open-file
   (mu4e--view-mime-part-to-temp-file (cdr (+mu4e-view-select-attachment)))))

(defun +mu4e-view-select-mime-part-action ()
  "Select a MIME part, and perform an action on it."
  (interactive)
  (let ((labeledparts (+mu4e-part-selectors (mu4e-view-mime-parts))))
    (if labeledparts
        (mu4e-view-mime-part-action
         (cadr (assoc (completing-read "Select part: " (mapcar #'car labeledparts))
                      labeledparts)))
      (user-error (mu4e-format "No parts found")))))

(defun +mu4e-view-save-all-attachments (&optional ask-dir)
  "Save all files from the current view buffer.

With ASK-DIR is non-nil, user can specify the target-directory; otherwise
one is determined using `mu4e-attachment-dir'."
  (interactive "P")
  (cl-letf (((symbol-function 'mu4e--completing-read)
             (lambda (_prompt candidates &rest _args)
               (mapcar (lambda (cand) (plist-get (cdr cand) :filename)) candidates))))
    (mu4e-view-save-attachments ask-dir)))

(defun +mu4e-register-account (label maildir letvars &optional default-p gmail-p)
  "Register a mu4e context named LABEL, located in MAILDIR.
LETVARS contains the alist of local variables with their values.
If DEFAULT-P is non-nil, the context is placed first and considered the default
one. If GMAIL-P is non-nil, addresses are saved to `+mu4e-gmail-accounts' to be
used later for Gmail specific actions."
  (let ((mail-address (alist-get 'user-mail-address letvars))
        (aliases (alist-get '+mu4e-account-aliases letvars)))
    (with-eval-after-load 'message
      ;; When I reply to my own message, don't include me in the "To" field (add all my addresses/aliases)
      (cl-callf append message-dont-reply-to-names (mapcar #'regexp-quote (append (ensure-list mail-address) (ensure-list aliases)))))
    (when gmail-p
      (with-eval-after-load 'me-mu4e-gmail
        (setq
         +mu4e-gmail-accounts
         (delete-dups
          (append
           +mu4e-gmail-accounts
           (mapcar (lambda (email) (cons email (concat (if (string-prefix-p "/" maildir) "" "/") maildir)))
                   (append (ensure-list mail-address) (ensure-list aliases)))))))))
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
    (add-to-list 'mu4e-contexts context (not default-p))
    context))

(cl-defun +org-msg-signature (firstname lastname &key (closing-phrase nil) (prefix nil) (suffix nil) (extra-lines nil))
  "Make an Org signature for FIRSTNAME and LASTNAME.
Possible key arguments are: :CLOSING-PHRASE, :PREFIX, :SUFFIX and a list
for :EXTRA-LINES."
  (concat
   "\n\n"
   (when closing-phrase (concat closing-phrase "\n\n"))
   "#+begin_signature"
   "\n"
   "-- "
   (when prefix (concat prefix " "))
   "*" (capitalize firstname) " " (upcase lastname) "*"
   (when suffix (concat ", " suffix))
   "\\\\\n"
   (string-join (ensure-list extra-lines) "\\\\\n")
   "\n"
   "#+end_signature"))

(defun +org-msg-make-signature (closing-phrase firstname lastname &rest lines)
  (concat
   "\n\n" closing-phrase "\n\n"
   "#+begin_signature"
   "\n"
   "-- *" (capitalize firstname) " " (upcase lastname) "*" "\\\\\n"
   (string-join lines "\\\\\n")
   "\n"
   "#+end_signature"))

(make-obsolete '+org-msg-make-signature '+org-msg-signature "8.6.1")

;; I always synchronize Spams with `mbsync' and index them with `mu'. However, I
;; don't like to see them all the time, I would rather jump to the spam folder
;; from time to time to check if a mail has been falsely classified as spam.
;; This function sets the `mu4e-bookmarks' to ignore the mails located in the
;; Spam or Junk folders.
(defun +mu4e-extras-ignore-spams-query (query)
  (let ((spam-filter "NOT maildir:/.*\\(spam\\|junk\\).*/"))
    (if (string-match-p spam-filter query)
        query
      (format "(%s) AND (%s)" query spam-filter))))

(defun +mu4e-extras-ignore-spams-in-bookmarks-setup ()
  (dolist (bookmark mu4e-bookmarks)
    (plist-put bookmark :query (+mu4e-extras-ignore-spams-query (plist-get bookmark :query)))))

;; I like to always BCC myself
(defun +mu4e--auto-bcc-h ()
  "Add BCC address from `+mu4e-auto-bcc-address'."
  (when +mu4e-auto-bcc-address
    (save-excursion (message-add-header (format "BCC: %s\n" (string-join (ensure-list +mu4e-auto-bcc-address) ", "))))))

(defun +mu4e--set-from-address-h ()
  "Choose the sender alias.
If the user defines multiple `+mu4e-account-aliases' for email aliases
within a context, set `user-mail-address' to an alias found in the TO or
FROM headers of the parent message if present, or prompt the user for a
preferred alias"
  (when-let* ((addresses (if (or mu4e-contexts +mu4e-account-aliases)
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
            (if (length= addresses 1)
                (car addresses)
              (completing-read "From: " addresses))))))

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

(defun +mu4e-save-message-at-point (&optional msg)
  "Copy MSG at point to somewhere else as <date>_<subject>.eml."
  (interactive)
  (let* ((msg (or msg (mu4e-message-at-point)))
         (target (format "%s_%s.eml"
                         (format-time-string "%F" (mu4e-message-field msg :date))
                         (+clean-file-name (or (mu4e-message-field msg :subject) "No subject") :downcase))))
    (copy-file
     (mu4e-message-field msg :path)
     (format "%s/%s" (or (when current-prefix-arg (read-directory-name "Copy message to: "))
                         mu4e-attachment-dir) target) 1)))

;; Based on: mu4e-action-view-in-browser
(defun +mu4e-view-save-mail-as-pdf (&optional msg skip-headers)
  "Save current MSG as PDF.
If SKIP-HEADERS is set, do not show include message headers."
  (interactive)
  (when-let* ((msg (or msg (mu4e-message-at-point))))
    (with-temp-buffer
      (insert-file-contents-literally
       (mu4e-message-readable-path msg) nil nil nil t)
      (run-hooks 'gnus-article-decode-hook)
      (let ((header (unless skip-headers
                      (cl-loop for field in '("from" "to" "cc" "date" "subject")
                               when (message-fetch-field field)
                               concat (format "%s: %s\n" (capitalize field) it))))
            (parts (mm-dissect-buffer t t)))
        ;; If singlepart, enforce a list.
        (when (and (bufferp (car parts))
                   (stringp (car (mm-handle-type parts))))
          (setq parts (list parts)))
        ;; Process the list
        ;; First, `+save-as-pdf' is set as browse-url function,
        ;; and the appropriate file output file name is bound to `+save-as-pdf-filename'
        (let ((browse-url-browser-function #'+save-as-pdf)
              (+save-as-pdf-filename
               (expand-file-name
                (format "%s_%s.pdf"
                        (format-time-string
                         "%F" (mu4e-message-field msg :date))
                        (+clean-file-name
                         (or (mu4e-message-field msg :subject) "No subject") t))
                mu4e-attachment-dir)))
          ;; `gnus-article-browse-html-parts' will try to display the text/html part
          ;; of the message, but as `+save-as-pdf' is used as browse-url function,
          ;; it will be called with the proper arguments.
          (unless (gnus-article-browse-html-parts parts header)
            ;; If the mail doesn't contain a text/html part, we save the plain-text message
            ;; and then we explicitly use `+save-as-pdf' to save it.
            (let ((outfile (make-temp-file "plaintext-mail-" nil ".txt")))
              (with-temp-file outfile
                (insert (mu4e-view-message-text msg)))
              (+save-as-pdf outfile t))))
        (mm-destroy-parts parts)))))

(defun +mu4e-extras-locks-setup ()
  "Setup locks for mu4e's server."
  (advice-add 'mu4e--server-kill :after (satch-defun +mu4e--unlock:after-a (&rest _) (+unlock 'mu)))
  (advice-add 'mu4e--server-start :after (satch-defun +mu4e--lock:after-a (&rest _) (+lock 'mu))))

(defun +mu4e-extras-setup ()
  (add-hook 'mu4e-compose-mode-hook '+mu4e--auto-bcc-h)
  (add-hook 'mu4e-compose-pre-hook '+mu4e--set-from-address-h)
  (add-hook 'message-send-hook #'+mu4e--check-for-subject-h)

  ;; Register actions
  (add-to-list 'mu4e-view-actions '("pdf" . +mu4e-view-save-mail-as-pdf))
  (add-to-list 'mu4e-view-actions '("Save message" . +mu4e-save-message-at-point)))


(provide 'me-mu4e-extras)

;;; me-mu4e-extras.el ends here
