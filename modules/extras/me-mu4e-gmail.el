;;; me-mu4e-gmail.el --- Better integration of mu4e with Gmail accounts -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-10-05
;; Last modified: 2026-05-30

;;; Commentary:

;; Adapted form Doom Emacs with several improvements

;;; Code:

(require 'mu4e)

(defcustom +mu4e-gmail-accounts nil
  "Gmail accounts that do not contain \"gmail\" in address and maildir.
An alist of Gmail addresses of the format
\\='((\"username@domain.com\" . \"account-maildir\"))
to which Gmail integrations (behind the `+gmail' flag of the `mu4e'
module) should be applied. See `+mu4e-msg-gmail-p' and
`mu4e-sent-messages-behavior'."
  :group 'minemacs-mu4e
  :type '(repeat (cons string string)))

(defun +mu4e-msg-gmail-p (msg)
  (let ((root-maildir
         (replace-regexp-in-string
          "/.*" "" (substring (mu4e-message-field msg :maildir) 1))))
    (or (member (concat "/" root-maildir)
                (seq-uniq (mapcar #'cdr +mu4e-gmail-accounts) #'string=))
        (string-match-p "gmail" root-maildir)
        (string-match-p "google" root-maildir))))

(defun +mu4e-sent-from-gmail-p (&optional msg)
  "Return the \"from\" address if it is in the registred Gmail accounts.
If MSG is provided, use it, else, extract the \"from\" field
from the envelope of the current message."
  (let ((from (or (plist-get (car (plist-get msg :from)) :email)
                  (message-sendmail-envelope-from))))
    (when (member from (mapcar #'car +mu4e-gmail-accounts))
      from)))

;; In this workflow, Gmail emails won't be moved at all. Only their flags/labels
;; are changed. Se we redefine the trash and refile marks not to do any moving.
;; However, the real magic happens in `+mu4e-gmail--fix-flags-h'. Gmail will
;; handle the rest.
(defun +mu4e--mark-seen (docid _msg target)
  (mu4e--server-move docid (mu4e--mark-check-target target) "+S-u-N"))

(defun +mu4e--gmail-sent-behavior ()
  "Don't save message for Gmail, the remote server takes care of this."
  (if (or (+mu4e-sent-from-gmail-p) (string-match-p "@gmail.com\\'" (message-sendmail-envelope-from)))
      'delete
    'sent))

;; Gmail-aware actions for `mu4e-marks'
(defun +mu4e-gmail--delete-action (docid msg target)
  (if (+mu4e-msg-gmail-p msg)
      (progn
        (mu4e-message "Unsupported delete operation for Gmail. Trashing instead.")
        (+mu4e--mark-seen docid msg target))
    (mu4e--server-remove docid)))

(defun +mu4e-gmail--trash-action (docid msg target)
  (if (+mu4e-msg-gmail-p msg)
      (+mu4e--mark-seen docid msg target)
    (mu4e--server-move docid (mu4e--mark-check-target target) (if mu4e-trash-without-flag "-N" "+T-N"))))

;; Refile will be my "archive" function
(defun +mu4e-gmail--refile-action (docid msg target)
  (if (+mu4e-msg-gmail-p msg)
      (+mu4e--mark-seen docid msg target)
    (mu4e--server-move docid (mu4e--mark-check-target target) "-N")))

(defun +mu4e-gmail-setup ()
  (setopt mu4e-sent-messages-behavior #'+mu4e--gmail-sent-behavior)

  (dolist (action '(delete trash refile))
    (let ((action-fn (intern (format "+mu4e-gmail--%s-action" action))))
      (let ((mark (assq action mu4e-marks)))
        (plist-put (cdr mark) :action action-fn))))

  ;; This hook correctly modifies gmail flags on emails when they are marked.
  ;; Without it, refiling (archiving), trashing/deleting, and flagging
  ;; (starring) email won't properly result in the corresponding gmail action,
  ;; since the marks are ineffectual otherwise.
  ;; NOTE: For these tricks to work properly, you need to:
  ;; 1. Go to your Gmail settings;
  ;; 2. In the "Forwarding and POP/IMAP" tab, go to "IMAP access" and make sure
  ;;    you've selected:
  ;;    - Under: "When I mark a message in IMAP as deleted:"
  ;;       -> Choose: "Auto-Expunge off - Wait for the client to update the server."
  ;;    - Under: "When a message is marked as deleted and expunged from the last visible IMAP folder:"
  ;;       -> Choose: "Move the message to the Trash"
  (add-hook
   'mu4e-mark-execute-pre-hook
   (+defun +mu4e-gmail--fix-flags-h (mark msg)
     (when (+mu4e-msg-gmail-p msg)
       (pcase mark
         ((or 'trash 'delete) (mu4e-action-retag-message msg "-\\Inbox,+\\Trash,-\\Draft,-\\Spam"))
         ('refile (mu4e-action-retag-message msg "-\\Inbox,-\\Spam"))
         ('flag (mu4e-action-retag-message msg "+\\Starred"))
         ('unflag (mu4e-action-retag-message msg "-\\Starred")))))))


(provide 'me-mu4e-gmail)

;;; me-mu4e-gmail.el ends here
