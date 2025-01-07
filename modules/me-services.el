;;; me-services.el --- Integration with web services -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;; Provide connectivity to Jira SOAP/REST services
(use-package jiralib
  :straight org-jira
  :commands (+jira-insert-ticket-id +jira-insert-ticket-link +jiralib-auto-login)
  :autoload +jira-get-ticket
  :init
  (defvar-local +jira-open-status '("open" "to do" "in progress"))
  (defvar-local +jira-commit-auto-insert-ticket-id-predicate nil)
  :config
  (defun +jira--ticket-annotation-fn (ticket)
    (let ((item (assoc ticket minibuffer-completion-table)))
      (when item (concat "    " (cdr item)))))

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
        (let ((completion-extra-properties '(:annotation-function +jira--ticket-annotation-fn)))
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
       (cond ((derived-mode-p 'org-mode)
              (format "[[%s][%s%s]]" link id (if with-summary (concat ": " summary) "")))
             ((derived-mode-p 'markdown-mode 'git-commit-mode 'markdown-ts-mode)
              (format "[%s%s](%s)" id (if with-summary (concat ": " summary) "") link))
             (t link)))))

  ;; INFO: Here is an example of a predicate function, it inserts a Jira ID
  ;; only if the project's remote contains the username "abougouffa"
  ;; (setq +jira-commit-auto-insert-ticket-id-predicate
  ;;       (lambda ()
  ;;         (when-let* ((urls (mapcar (lambda (remote) (string-trim-right (shell-command-to-string (format "git config --get remote.%s.url" remote)))) (magit-list-remotes))))
  ;;           (cl-some (apply-partially #'string-match-p (rx (or "github.com/abougouffa" "gitlab.com/abougouffa"))) urls))))

  (defun +jira-commit-auto-insert-ticket-id ()
    "Insert a ticket ID at the beginning of the commit if the first line is empty.

This function is meant to be hooked to `git-commit-mode-hook' for
projects to uses the convention of commit messages like:

\"JIRA-TICKET-ID: Commit message\"."
    (when (and (+first-line-empty-p) ; Do not auto insert if the commit message is not empty (ex. amend)
               +jira-commit-auto-insert-ticket-id-predicate
               (funcall +jira-commit-auto-insert-ticket-id-predicate))
      (goto-char (point-min))
      (insert "\n")
      (goto-char (point-min))
      (let ((pt (point)))
        (+jira-insert-ticket-id nil)
        (when (/= pt (point))
          (insert ": ")))))

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


;; Bring Jira and Org mode together
(use-package org-jira
  :straight (:host github :repo "ahungry/org-jira")
  :custom
  (org-jira-working-dir (+directory-ensure org-directory "jira/")))


;; Edit Confluence wiki pages in Emacs
(use-package tributary
  :straight (:host github :repo "mrkrd/tributary" :files (:defaults "confluence.rnc"))
  :commands (tributary-mode tributary-push tributary-pull-id tributary-pull-url))


;; Stack Exchange for Emacs
(use-package sx
  :straight t
  :custom
  (sx-cache-directory (concat minemacs-cache-dir "sx/")))


;; Paste text to pastebin-like services
(use-package webpaste
  :straight t
  :custom
  (webpaste-provider-priority '("paste.mozilla.org" "dpaste.org")))


(provide 'me-services)

;;; me-services.el ends here
