;;; me-services.el --- Integration with web services -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-10-02
;; Last modified: 2025-06-11

;;; Commentary:

;;; Code:

;; Provide connectivity to Jira SOAP/REST services
(use-package jiralib
  :straight (:host github :repo "ahungry/org-jira" :files ("jiralib.el"))
  :commands (+jira-insert-ticket-id +jira-insert-ticket-link +jiralib-auto-login)
  :autoload +jira-get-ticket
  :init
  (defvar-local +jira-open-status '("open" "to do" "in progress"))
  (defvar-local +jira-commit-auto-insert-ticket-id-function nil)
  :config
  (defun +jira-get-ticket ()
    "Get ticket ID, choose from states defined in `+jira-open-status'."
    (when-let* ((user (or jiralib-user-login-name jiralib-user))
                (issues (jiralib-do-jql-search
                         (format "assignee=\"%s\" AND status in (%s)" user
                                 (string-join (mapcar (apply-partially #'format "%S") +jira-open-status) ", "))))
                (tickets (mapcar (lambda (ticket)
                                   (cons (cdr (assoc 'key ticket))
                                         (cdr (assoc 'summary (cdr (assoc 'fields ticket))))))
                                 issues)))
      (if (length= tickets 1)
          (car tickets)
        (let ((completion-extra-properties
               '(:annotation-function
                 ,(lambda (ticket)
                    (when-let* ((desc (alist-get ticket minibuffer-completion-table)))
                      (concat (make-string (- 15 (length ticket)) ?\ ) (propertize desc 'face 'font-lock-comment-face)))))))
          (assoc (completing-read "Select ticket: " tickets) tickets #'string=)))))

  (defun +jira-insert-ticket-id (with-summary)
    "Insert ticket ID, optionally WITH-SUMMARY.
This depends on the states defined in `+jira-open-status'."
    (interactive "P")
    (when-let* ((ticket (+jira-get-ticket)))
      (insert (format "%s%s" (car ticket) (if with-summary (concat ": " (cdr ticket)) "")))))

  (defun +jira-insert-ticket-link (with-summary)
    "Insert ticket link, optionally WITH-SUMMARY.
This depends on the states defined in `+jira-open-status'.
The link style depends on the current major mode."
    (interactive "P")
    (let* ((ticket (+jira-get-ticket))
           (id (car ticket))
           (summary (cdr ticket))
           (link (format "%s/browse/%s" jiralib-url id)))
      (insert
       (cond ((derived-mode-p '(org-mode))
              (format "[[%s][%s%s]]" link id (if with-summary (concat ": " summary) "")))
             ((derived-mode-p '(markdown-mode git-commit-mode markdown-ts-mode))
              (format "[%s%s](%s)" id (if with-summary (concat ": " summary) "") link))
             (t link)))))

  (defun +jira-commit-auto-insert-ticket-id ()
    "Insert a ticket ID at the beginning of the commit if the first line is empty.

This function is meant to be hooked to `git-commit-mode-hook' for
projects to uses the convention of commit messages like:

\"JIRA-TICKET-ID: Commit message\"."
    (when (and (+first-line-empty-p) ; Do not auto insert if the commit message is not empty (ex. amend)
               +jira-commit-auto-insert-ticket-id-function)
      (goto-char (point-min))
      (insert "\n")
      (goto-char (point-min))
      (funcall +jira-commit-auto-insert-ticket-id-function)))

  (defun +jiralib-auto-login ()
    "Auto login to Jira using credentials from `auth-source'."
    (interactive)
    (unless jiralib-token
      (when-let* ((host (if (string-empty-p jiralib-host)
                            (url-host (url-generic-parse-url jiralib-url))
                          jiralib-host))
                  (auth (car (auth-source-search :host host)))
                  (user (plist-get auth :user))
                  (pass (funcall (plist-get auth :secret))))
        (jiralib-login user pass)))))


;; Paste text to pastebin-like services
(use-package webpaste
  :straight t
  :custom
  (webpaste-provider-priority '("paste.mozilla.org" "dpaste.org")))


;; Simply shortening URLs using the is.gd service
(use-package isgd
  :straight t
  :custom
  (isgd-ask-custom-url t))


;; Work seamlessly with GitHub gists from Emacs
(use-package igist
  :straight t
  :config
  (advice-add ; BUG+FIX: Don't save the Gist unless it has been modified
   'igist-save-gist-buffer :around
   (satch-defun igist--check-if-modified:around-a (orig-fn buffer &optional callback)
     (when (igist-gist-modified-p buffer)
       (funcall orig-fn buffer callback)))))


(provide 'me-services)

;;; me-services.el ends here
