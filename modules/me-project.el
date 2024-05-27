;;; me-project.el --- Projects stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package consult-project-extra
  :straight t
  :init
  (+map! :infix "p" ;; project
    "p" #'consult-project-extra-find
    "P" #'consult-project-extra-find-other-window))

(use-package find-file-in-project
  :straight t
  :custom
  (ffip-use-rust-fd (and (executable-find "fd") t)))

(use-package compile-multi
  :straight t)

(use-package compile-multi-embark
  :straight t
  :after embark
  :init
  (compile-multi-embark-mode 1))

(use-package consult-compile-multi
  :straight t
  :after consult
  :init
  (consult-compile-multi-mode 1))

(use-package projection
  :straight t
  :hook (ibuffer . ibuffer-projection-set-filter-groups)
  :bind-keymap ("C-x P" . projection-map)
  :init
  ;; This ensures that `ibuffer-projection-set-filter-groups' takes effect
  (+add-hook! ibuffer (run-at-time 0.1 nil (lambda () (call-interactively #'ibuffer-update)))))

(use-package projection-multi
  :straight t
  :init
  (+map! "pC" #'projection-multi-compile))

(use-package projection-multi-embark
  :straight t
  :after embark projection-multi
  :init
  (projection-multi-embark-setup-command-map))

(use-package projection-dape
  :straight t
  :init
  (+map! :infix "d"
    "D" #'projection-dape))

(use-package project-x
  :straight (:host github :repo "karthink/project-x")
  :after project
  :commands project-x-window-state-save project-x-window-state-load
  :custom
  (project-x-window-list-file (concat minemacs-local-dir "project-x/project-window-list.el"))
  (project-x-local-identifier '("project.el" ".project.el"))
  :init
  (project-x-mode 1))

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
           (completing-read "Select ticket: " tickets)))))))


(provide 'me-project)

;;; me-project.el ends here
