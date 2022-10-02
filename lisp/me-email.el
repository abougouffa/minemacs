;; -*- lexical-binding: t; -*-

(defconst MU4E-LOAD-PATH "/usr/share/emacs/site-lisp/mu4e/")

(use-package mu4e
  :when (file-directory-p MU4E-LOAD-PATH)
  :load-path MU4E-LOAD-PATH
  :commands mu4e mu4e-compose-new
  :general
  (me-global-def "om" '(mu4e :which-key "Mu4e"))
  :config
  (require 'mu-mu4e-extras)

  ;; (me-local-def :keymaps mu4e-compose-mode-map
  ;;   "s" #'message-send-and-exit
  ;;   "d" #'message-kill-buffer
  ;;   "S" #'message-dont-send)

  ;; (me-map-def :keymaps 'mu4e-view-mode-map
  ;;   "A" #'+mu4e-view-select-mime-part-action
  ;;   "p" #'mu4e-view-save-attachments
  ;;   "P" #'+mu4e-view-save-all-attachments
  ;;   "o" #'+mu4e-view-open-attachment)

  (setq mu4e-confirm-quit nil
        mu4e-view-use-gnus t
        mu4e-view-show-images t
        mu4e-view-show-addresses t
        mu4e-view-image-max-width 800
        mu4e-search-results-limit 1000
        mu4e-index-cleanup t
        mu4e-attachment-dir (expand-file-name "~/Downloads/mu4e-attachements")
        mu4e-update-interval (* 3 60) ;; Every 3 min
        mu4e-context-policy 'pick-first ;; Start with the first context
        mu4e-compose-context-policy 'ask ;; Always ask which context to use when composing a new mail
        mu4e-sent-messages-behavior 'sent
        mu4e-get-mail-command "mbsync -a" ;; Use mbsync to get mails
        mu4e-index-update-error-warning nil ;; Do not show warning after update
        mu4e-main-hide-personal-addresses t ;; No need to display a long list of my own addresses!
        mu4e-change-filenames-when-moving t
        mu4e-completing-read-function (if (featurep 'vertico) #'completing-read #'ido-completing-read))

  (setq sendmail-program (executable-find "msmtp") ;; Use msmtp to send mails
        send-mail-function #'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail
        message-sendmail-envelope-from 'obey-mail-envelope-from
        message-kill-buffer-on-exit t ;; close after sending
        mail-envelope-from 'header
        mail-specify-envelope-from t
        mail-user-agent 'mu4e-user-agent
        message-mail-user-agent 'mu4e-user-agent)

  ;; Setup UI
  (if (display-graphic-p)
      (progn
        (require 'me-mu4e-ui)
        (me-mu4e--ui-setup))
    (add-hook
     'server-after-make-frame-hook
     (defun +mu4e--setup-ui-hook ()
       (when (display-graphic-p)
         (require 'me-mu4e-ui)
         (me-mu4e--ui-setup)
         (remove-hook 'server-after-make-frame-hook
                      #'+mu4e--setup-ui-hook)))))

  ;; Html mails might be better rendered in a browser
  (add-to-list 'mu4e-view-actions '("View in browser" . mu4e-action-view-in-browser))
  (when (fboundp 'make-xwidget)
    (add-to-list 'mu4e-view-actions '("xwidgets view" . mu4e-action-view-in-xwidget)))

  (add-hook 'message-send-hook #'+mu4e-check-for-subject))

(with-eval-after-load 'evil
  ;; As +mu4e~main-action-str-prettier replaces [k]ey with key q]uit should become quit
  (setq evil-collection-mu4e-end-region-misc "quit")

  (defun org-msg-mode (&optional _)
    "Dummy function."
    (message "Enable the +org mu4e flag to use org-msg-mode.")))


(use-package org-msg
  :straight t
  :after mu4e)


(use-package mu4e-alert
  :straight t
  :after mu4e
  :config
  (setq mu4e-alert-icon "/usr/share/icons/Papirus/64x64/apps/mail-client.svg"))


(use-package evil-mu4e
  :disabled t
  :straight t
  :after mu4e)


(provide 'me-email)
