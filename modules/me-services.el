;;; me-services.el --- Integration with web services -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package org-jira
  :straight (:host github :repo "ahungry/org-jira"))

(use-package jiralib
  :straight org-jira
  :commands +jira-insert-ticket-id +jira-insert-ticket-link +jiralib-auto-login
  :autoload +jira-get-ticket
  :init
  (defvar-local +jira-open-status '("open" "to do" "in progress"))
  :config
  (defun +jira--ticket-annotation-fn (ticket)
    (let ((item (assoc ticket minibuffer-completion-table)))
      (when item (concat "    " (cdr item)))))

  (defun +jira-get-ticket ()
    "Insert ticket ID, choose from states defined in `+jira-open-status'."
    (interactive)
    (when-let* ((issues (jiralib-do-jql-search (format "assignee=\"%s\" AND status in (%s)"
                                                       jiralib2-user-login-name
                                                       (string-join (mapcar (apply-partially #'format "%S") +jira-open-status) ", "))))
                (tickets (mapcar (lambda (ticket) (cons (cdr (assoc 'key ticket)) (cdr (assoc 'summary (cdr (assoc 'fields ticket)))))) issues)))
      (if (length= tickets 1)
          (car tickets)
        (let ((completion-extra-properties '(:annotation-function +jira--ticket-annotation-fn)))
          (assoc (completing-read "Select ticket: " tickets) tickets #'string=)))))

  (defun +jira-insert-ticket-id (with-summary)
    "Insert ticket ID, choose from states defined in `+jira-open-status'."
    (interactive "P")
    (when-let* ((ticket (car tickets)))
      (insert (format "%s%s" (car ticket) (if with-summary (concat ": " (cdr ticket)) "")))))

  (defun +jira-insert-ticket-link (with-summary)
    "Insert ticket link, choose from states defined in `+jira-open-status'."
    (interactive "P")
    (let* ((ticket (+jira-get-ticket))
           (id (car ticket))
           (summary (cdr ticket))
           (link (format "%s/browse/%s" jiralib-url id)))
      (insert
       (cond ((derived-mode-p 'org-mode) (format "[[%s][%s%s]]" link id (if with-summary (concat ": " summary) "")))
             ((derived-mode-p 'markdown-mode 'git-commit-mode) (format "[%s%s](%s)" id (if with-summary (concat ": " summary) "") link))
             (t link)))))

  (defun +jiralib-auto-login ()
    "Auto login to Jira using credentials from `auth-source'."
    (interactive)
    (unless jiralib-token
      (when-let* ((host (if (string= jiralib-host "")
                            (url-host (url-generic-parse-url jiralib-url))
                          jiralib-host))
                  (auth (car (auth-source-search :host jiralib-url)))
                  (user (plist-get auth :user))
                  (pass (funcall (plist-get auth :secret))))
        (jiralib-login user pass)))))

(use-package tributary
  :straight (:host github :repo "mrkrd/tributary" :files (:defaults "confluence.rnc"))
  :commands tributary-mode tributary-push tributary-pull-id tributary-pull-url)


(provide 'me-services)

;;; me-services.el ends here
