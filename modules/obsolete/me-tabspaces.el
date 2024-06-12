;;; me-tabspaces.el --- Code coverage -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:


(use-package tabspaces
  :straight t
  :after minemacs-loaded
  :hook (tabspaces-mode . +consult-tabspaces-setup)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-initialize-project-with-todo nil)
  (tabspaces-default-tab "*default*")
  (tabspaces-include-buffers '("*scratch*"))
  (tabspaces-session t)
  (tabspaces-session-file (+directory-ensure minemacs-local-dir "tabspaces/session.el"))
  :config
  ;; Ensure reading project list
  (require 'project)
  (project--ensure-read-project-list)

  (defun +consult-tabspaces-setup ()
    "Deactivate isolated buffers when not using tabspaces."
    (require 'consult)
    (cond (tabspaces-mode
           ;; hide full buffer list (still available with "b")
           (consult-customize consult--source-buffer :hidden t :default nil)
           (add-to-list 'consult-buffer-sources '+consult--source-workspace))
          (t
           ;; reset consult-buffer to show all buffers
           (consult-customize consult--source-buffer :hidden nil :default t)
           (setq consult-buffer-sources (remove #'+consult--source-workspace consult-buffer-sources)))))

  (with-eval-after-load 'consult
    ;; Hide full buffer list (still available with "b" prefix)
    (consult-customize consult--source-buffer :hidden t :default nil)
    ;; Set consult-workspace buffer list
    (defvar +consult--source-workspace
      (list :name "Workspace Buffers"
            :narrow   '(?w . "Workspace")
            :history  'buffer-name-history
            :category 'buffer
            :state    #'consult--buffer-state
            :default  t
            :items
            (lambda ()
              (consult--buffer-query
               :predicate #'tabspaces--local-buffer-p
               :sort      'visibility
               :as        #'buffer-name))))

    (add-to-list 'consult-buffer-sources '+consult--source-workspace))

  ;; Switch to the scratch buffer after creating a new workspace
  (advice-add
   'tabspaces-switch-or-create-workspace :around
   (satch-defun +tabspaces--switch-to-scratch-after-create:around-a (origfn &rest workspace)
     (let ((before-list (tabspaces--list-tabspaces)))
       (apply origfn workspace)
       ;; Created a new empty workspace
       (when-let ((new-ws (cl-set-difference (tabspaces--list-tabspaces) before-list :test #'string=)))
         (+scratch-open-buffer nil nil 'same-window)))))

  (tabspaces-mode 1)

  ;; Rename the first tab to `tabspaces-default-tab'
  (tab-bar-rename-tab tabspaces-default-tab))


(provide 'obsolete/me-tabspaces)
;;; me-tabspaces.el ends here
