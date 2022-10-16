;; -*- lexical-binding: t; -*-

;; Taken form Doom Emacs

(defcustom +mu4e-gmail-accounts nil
  "Gmail accounts that do not contain \"gmail\" in address and maildir.
An alist of Gmail addresses of the format \((\"username@domain.com\" . \"account-maildir\"))
to which Gmail integrations (behind the `+gmail' flag of the `mu4e' module) should be applied.
See `+mu4e-msg-gmail-p' and `mu4e-sent-messages-behavior'.")

(defun +mu4e-msg-gmail-p (msg)
  (let ((root-maildir
         (replace-regexp-in-string
          "/.*" "" (substring (mu4e-message-field msg :maildir) 1))))
    (or (string-match-p "gmail" root-maildir)
        (string-match-p "google" root-maildir)
        (member (concat "/" root-maildir) (mapcar #'cdr +mu4e-gmail-accounts)))))

;; In my workflow, emails won't be moved at all. Only their flags/labels are
;; changed. Se we redefine the trash and refile marks not to do any moving.
;; However, the real magic happens in `+mu4e-gmail-fix-flags-h'.
;; Gmail will handle the rest.
(defun +mu4e--mark-seen (docid _msg target)
  (mu4e--server-move docid (mu4e--mark-check-target target) "+S-u-N"))

(defvar +mu4e--last-invalid-gmail-action 0.0)

(defun +mu4e-gmail-setup ()
  ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
  (setq mu4e-sent-messages-behavior
        (lambda () ;; TODO make use +mu4e-msg-gmail-p
          (if (or (string-match-p "@gmail.com\\'" (message-sendmail-envelope-from))
                  (member (message-sendmail-envelope-from)
                          (mapcar #'car +mu4e-gmail-accounts)))
              'delete 'sent)))

  (setq mu4e-marks (delq (assq 'delete mu4e-marks) mu4e-marks))

  (setf (alist-get 'delete mu4e-marks)
        (list
         :char '("D" . "✘")
         :prompt "Delete"
         :show-target (lambda (_target) "delete")
         :action (lambda (docid msg target)
                   (if (+mu4e-msg-gmail-p msg)
                       (progn (message "The delete operation is invalid for Gmail accounts. Trashing instead.")
                              (+mu4e--mark-seen docid msg target)
                              (when (< 2.0 (- (float-time) +mu4e--last-invalid-gmail-action))
                                (sit-for 1))
                              (setq +mu4e--last-invalid-gmail-action (float-time)))
                     (mu4e--server-remove docid))))
        (alist-get 'trash mu4e-marks)
        (list :char '("d" . "▼")
              :prompt "dtrash"
              :dyn-target (lambda (_target msg) (mu4e-get-trash-folder msg))
              :action (lambda (docid msg target)
                        (if (+mu4e-msg-gmail-p msg)
                            (+mu4e--mark-seen docid msg target)
                          (mu4e--server-move docid (mu4e--mark-check-target target) "+T-N"))))
        ;; Refile will be my "archive" function.
        (alist-get 'refile mu4e-marks)
        (list :char '("r" . "▼")
              :prompt "rrefile"
              :dyn-target (lambda (_target msg) (mu4e-get-refile-folder msg))
              :action (lambda (docid msg target)
                        (if (+mu4e-msg-gmail-p msg)
                            (+mu4e--mark-seen docid msg target)
                          (mu4e--server-move docid (mu4e--mark-check-target target) "-N")))))

  ;; This hook correctly modifies gmail flags on emails when they are marked.
  ;; Without it, refiling (archiving), trashing, and flagging (starring) email
  ;; won't properly result in the corresponding gmail action, since the marks
  ;; are ineffectual otherwise.
  (add-hook 'mu4e-mark-execute-pre-hook
            (defun +mu4e-gmail--fix-flags-h (mark msg)
              (when (+mu4e-msg-gmail-p msg)
                (pcase mark
                  ((or 'trash 'delete) (mu4e-action-retag-message msg "-\\Inbox,+\\Trash,-\\Draft"))
                  ('refile (mu4e-action-retag-message msg "-\\Inbox"))
                  ('flag (mu4e-action-retag-message msg "+\\Starred"))
                  ('unflag (mu4e-action-retag-message msg "-\\Starred")))))))


(provide 'me-mu4e-gmail)
