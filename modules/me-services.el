;;; me-services.el --- Integration with web services -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:


(use-package jiralib2
  :straight t
  :commands +jira-insert-ticket-id +jira-insert-ticket-link
  :init
  (defvar-local +jira-open-status '("open" "to do" "in progress"))
  :config
  (defun +jira--ticket-annotation-fn (ticket)
    (let ((item (assoc ticket minibuffer-completion-table)))
      (when item (concat "    " (cdr item)))))

  (defun +jira-insert-ticket-id ()
    "Insert ticket ID, choose from states defined in `+jira-open-status'."
    (interactive)
    (when-let* ((issues (jiralib2-jql-search (format "assignee=\"%s\" AND status in (%s)"
                                                     jiralib2-user-login-name
                                                     (string-join (mapcar (apply-partially #'format "%S") +jira-open-status) ", "))))
                (tickets (mapcar (lambda (t) (cons (cdr (assoc 'key t)) (cdr (assoc 'summary (cdr (assoc 'fields t)))))) issues)))
      (insert
       (if (length= tickets 1)
           (car (car tickets))
         (let ((completion-extra-properties '(:annotation-function +jira--ticket-annotation-fn)))
           (completing-read "Select ticket: " tickets))))))

  (defun +jira-insert-ticket-link ()
    "Insert ticket link, choose from states defined in `+jira-open-status'."
    (interactive)
    (let* ((id (with-temp-buffer (+jira-insert-ticket-id) (buffer-string)))
           (link (format "%s/browse/%s" jiralib2-url id)))
      (insert
       (cond ((derived-mode-p 'org-mode) (format "[[%s][%s]]" link id))
             ((derived-mode-p 'markdown-mode 'git-commit-mode) (format "[%s](%s)" id link))
             (t link))))))

(use-package org-jira
  :straight (:host github :repo "ahungry/org-jira"))

(use-package tributary
  :straight (:host github :repo "mrkrd/tributary" :files (:defaults "confluence.rnc")))


(provide 'me-services)

;;; me-services.el ends here
