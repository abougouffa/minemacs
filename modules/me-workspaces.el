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
  :after minemacs-loaded
  :hook (tabspaces-mode . +consult-tabspaces-setup)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
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
     (let ((length-before (length (tabspaces--list-tabspaces))))
       (apply origfn workspace)
       (when (length> (tabspaces--list-tabspaces) length-before)
         (switch-to-buffer (get-scratch-buffer-create))))))

  (tabspaces-mode 1)

  ;; Rename the first tab to `tabspaces-default-tab'
  (tab-bar-rename-tab tabspaces-default-tab))

(use-package tab-bar
  :straight (:type built-in)
  :hook (minemacs-after-startup . tab-bar-mode)
  :custom
  (tab-bar-format '(tab-bar-format-history
                    tab-bar-format-tabs
                    tab-bar-separator))
  (tab-bar-tab-hints t)
  (tab-bar-tab-name-format-function #'+tab-bar-tab-spaced-name-format)
  (tab-bar-close-button-show nil)
  (tab-bar-show nil)
  :config
  (defun +tab-bar-tab-spaced-name-format (tab i)
    (let ((current-p (eq (car tab) 'current-tab)))
      (propertize
       (concat (if tab-bar-tab-hints (format " %c " (+ ?① (1- i)) " "))
               (alist-get 'name tab)
               (or (and tab-bar-close-button-show
                        (not (eq tab-bar-close-button-show
                                 (if current-p 'non-selected 'selected)))
                        tab-bar-close-button)
                   ""))
       'face (funcall tab-bar-tab-face-function tab)))))


(provide 'me-workspaces)

;;; me-workspaces.el ends here
