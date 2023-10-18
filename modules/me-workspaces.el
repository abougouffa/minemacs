;;; workspaces.el --- Windows, workspaces (via tab-bar & tab-line) -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;; IDEA:
;; - github.com/fritzgrabo/project-tab-groups
;; - github.com/florommel/bufferlo
;; - www.rousette.org.uk/archives/using-the-tab-bar-in-emacs

;;; Code:

(use-package tabspaces
  :straight t
  ;; TEMP: Use a stable commit until I open a PR on upstream (WIP)
  :pin-ref "3ecbd49fbd3c9c628a838957dc82547f1269b455"
  :after minemacs-loaded
  :hook (tabspaces-mode . +consult-tabspaces-setup)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-initialize-project-with-todo nil)
  (tabspaces-default-tab "*default*")
  (tabspaces-include-buffers '("*scratch*"))
  (tabspaces-session t)
  (tabspaces-session-file (+directory-ensure minemacs-local-dir "tabspaces/session.el"))
  :init
  (+map! :infix "q"
    "t" #'tabspaces-save-session
    "T" #'tabspaces-restore-session
    "p" #'tabspaces-save-current-project-session)
  (+map! :infix "TAB"
    "TAB" '(tabspaces-switch-or-create-workspace :w "Switch or create")
    "o" '(tabspaces-open-or-create-project-and-workspace :wk "Open or create project")
    "f" '(tabspaces-project-switch-project-open-file :wk "Switch project & open file")
    "d" #'tabspaces-close-workspace
    "b" #'tabspaces-switch-to-buffer
    "t" #'tabspaces-switch-buffer-and-tab
    "C" #'tabspaces-clear-buffers
    "r" #'tabspaces-remove-current-buffer
    "R" #'tabspaces-remove-selected-buffer
    "k" #'(tabspaces-kill-buffers-close-workspace :wk "Kill buffers & close WS"))
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
   (defun +tabspaces--switch-to-scratch-after-create-a (origfn &rest workspace)
     (let ((before-list (tabspaces--list-tabspaces)))
       (apply origfn workspace)
       ;; Created a new empty workspace
       (when-let ((new-ws (cl-set-difference (tabspaces--list-tabspaces) before-list :test #'string=)))
         (+scratch-open-buffer nil nil 'same-window)))))

  (tabspaces-mode 1)

  ;; Rename the first tab to `tabspaces-default-tab'
  (tab-bar-rename-tab tabspaces-default-tab))


(provide 'me-workspaces)

;;; me-workspaces.el ends here
