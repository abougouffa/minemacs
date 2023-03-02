;;; workspaces.el --- Windows, workspaces (via tab-bar & tab-line) -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

;; IDEA:
;; - https://github.com/fritzgrabo/tab-bar-groups
;; - https://github.com/fritzgrabo/project-tab-groups
;; - https://github.com/florommel/bufferlo
;; - https://www.rousette.org.uk/archives/using-the-tab-bar-in-emacs


(use-package tabspaces
  :straight t
  :hook (minemacs-after-startup . tabspaces-mode)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  (tabspaces-session t)
  (tabspaces-session-auto-restore t)
  (tabspaces-session-file (+directory-ensure (concat minemacs-local-dir "tabspaces/session.el")))
  :init
  (+map :infix "q"
    "t" #'tabspaces-save-session
    "T" #'tabspaces-restore-session)
  (+map :infix "TAB"
    "o" #'tabspaces-open-or-create-project-and-workspace
    "f" #'tabspaces-project-switch-project-open-file
    "d" #'tabspaces-close-workspace
    "s" #'tabspaces-switch-or-create-workspace
    "b" #'tabspaces-switch-to-buffer
    "t" #'tabspaces-switch-buffer-and-tab
    "C" #'tabspaces-clear-buffers
    "r" #'tabspaces-remove-current-buffer
    "R" #'tabspaces-remove-selected-buffer
    "k" #'tabspaces-kill-buffers-close-workspace)
  :config
  (defun +consult-tabspaces ()
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

  (add-hook 'tabspaces-mode-hook #'+consult-tabspaces)

  (with-eval-after-load 'consult
    ;; hide full buffer list (still available with "b" prefix)
    (consult-customize consult--source-buffer :hidden t :default nil)
    ;; set consult-workspace buffer list
    (defvar +consult--source-workspace
      (list :name     "Workspace Buffers"
            :narrow   ?w
            :history  'buffer-name-history
            :category 'buffer
            :state    #'consult--buffer-state
            :default  t
            :items    (lambda () (consult--buffer-query
                                  :predicate #'tabspaces--local-buffer-p
                                  :sort 'visibility
                                  :as #'buffer-name)))
      "Set workspace buffer list for consult-buffer.")

    (add-to-list 'consult-buffer-sources '+consult--source-workspace)))

(use-package tab-bar
  :straight (:type built-in)
  :custom
  (tab-bar-format '(tab-bar-format-history
                    tab-bar-format-tabs
                    tab-bar-separator))
  (tab-bar-tab-hints t)
  (tab-bar-tab-name-format-function #'+tab-bar-tab-spaced-name-format)
  (tab-bar-close-button-show t)
  (tab-bar-show nil) ; until eldoc-box gets fixed
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
