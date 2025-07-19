;;; me-enlight.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@tznvy.pbz")
;; Created: 2025-07-19
;; Last modified: 2025-07-19

;;; Commentary:

;;; Code:


;; Highly customizable startup screen for Emacs
(use-package enlight
  :straight (:host github :repo "ichernyshovvv/enlight")
  :hook (enlight-mode . +enlight-responsive-h)
  :custom
  (enlight-content
   (enlight-menu
    `(("Org Mode"
       ("Org-Agenda (today)" (org-agenda nil "a") "a")
       ("Org directory" (dired org-directory) "o"))
      ("Emacs"
       ("Calendar" calendar "c"))
      ,@(unless (+package-disabled-p 'denote 'me-notes)
          `(("Notes"
             ("Denote" denote "n")
             ("Open or create" denote-open-or-create "N")
             ("Journal new or existing" denote-journal-new-or-existing-entry "j"))))
      ("Projects"
       ("Switch to project" project-switch-project "p"))
      ,@(unless (+package-disabled-p 'easysession 'me-ui)
          `(("Session"
             ("Load session" easysession-load "l")
             ("Switch to session" easysession-switch-to "s")))))))
  :init
  (if minemacs-started-with-extra-args-p
      (enlight-open)
    (setq initial-buffer-choice #'enlight))
  :config
  (defun +enlight-responsive-h ()
    (satch-add-hook
     '(window-size-change-functions window-state-change-functions)
     (satch-defun +enlight--recenter-h (&optional _frame)
       ;; When Enlight's buffer is visible, we recall `enlight' to refresh/recenter it
       (and (get-buffer-window enlight-buffer-name) (enlight)))
     nil 'local)))


(provide 'obsolete/me-enlight)
;;; me-enlight.el ends here
