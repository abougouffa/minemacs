;; -*- lexical-binding: t; -*-

(defconst MU4E-LOAD-PATH "/usr/share/emacs/site-lisp/mu4e/")
(defconst MU4E-P (me-all (list (executable-find "mu")
                               (executable-find "mbsync")
                               (executable-find "msmtp")
                               (file-directory-p MU4E-LOAD-PATH))))

(use-package mu4e
  :when MU4E-P
  :load-path MU4E-LOAD-PATH
  :commands mu4e mu4e-compose-new mu4e--start
  :general
  (me-map "om" '(mu4e :which-key "Mu4e"))
  :config
  (require 'me-mu4e-ui)
  (require 'me-mu4e-gmail)
  (require 'me-mu4e-extras)

  (me-map-local :keymaps '(mu4e-compose-mode-map org-msg-edit-mode-map)
    "s" #'message-send-and-exit
    "d" #'message-kill-buffer
    "S" #'message-dont-send)
  (me-map-key :keymaps 'mu4e-view-mode-map
    "p" #'mu4e-view-save-attachments)

  (setq mu4e-confirm-quit nil
        mu4e-search-results-limit 1000
        mu4e-index-cleanup t
        mu4e-attachment-dir (expand-file-name "~/Downloads/mu4e-attachements")
        mu4e-update-interval (* 3 60) ;; Every 3 min
        mu4e-context-policy 'pick-first ;; Start with the first context
        mu4e-compose-context-policy 'ask ;; Always ask which context to use when composing a new mail
        mu4e-sent-messages-behavior 'sent ;; Will be overwritten for Gmail accounts
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
        message-mail-user-agent 'mu4e-user-agent
        message-kill-buffer-on-exit t ;; close after sending
        mail-envelope-from 'header
        mail-specify-envelope-from t
        mail-user-agent 'mu4e-user-agent
        read-mail-command 'mu4e)

  ;; Setup UI
  (if (display-graphic-p)
      (me-mu4e--ui-setup)
    (add-hook 'server-after-make-frame-hook
              (defun +mu4e--setup-ui-h ()
                (when (display-graphic-p)
                  (me-mu4e--ui-setup)
                  (remove-hook 'server-after-make-frame-hook
                               #'+mu4e--setup-ui-h)))))

  (+mu4e-extras-setup) ;; Extra features
  (+mu4e-gmail-setup)) ;; Gmail specifics


(use-package org-msg
  :straight t
  :after mu4e
  :config
  (me-map-key :keymaps 'org-msg-edit-mode-map
    "TAB" #'org-msg-tab
    "gg"  #'org-msg-goto-body)
  (me-map-local :keymaps 'org-msg-edit-mode-map
    "a"  '(nil :which-key "attach")
    "aa" '(org-msg-attach-attach :which-key "Attach")
    "ad" '(org-msg-attach-delete :which-key "Delete")
    "p"  '(org-msg-preview :which-key "Preview"))

  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil tex:dvipng"
        org-msg-startup "hidestars indent inlineimages"
        org-msg-greeting-name-limit 3
        org-msg-default-alternatives '((new . (utf-8 html))
                                       (reply-to-text . (utf-8 html))
                                       (reply-to-html . (utf-8 html)))
        org-msg-convert-citation t
        ;; TODO: See https://regex101.com/r/EtaiSP/4 for a better alternative!
        org-msg-attached-file-reference
        (rx (or (seq "attach" (or "ment" "ed"))
                (seq "enclosed")
                (seq "attach" (any ?é ?e) (? "e") (? "s"))
                (seq "ci" (or " " "-") "joint" (? "e")) ;; ci-joint
                (seq (or (seq "pi" (any ?è ?e) "ce") "fichier" "document") (? "s") (+ (or " " eol)) "joint" (? "e") (? "s")) ;; pièce jointe
                (seq (or (seq space "p" (zero-or-one (any ?- ?.)) "j" space)))))) ;; p.j

  ;; Setup Org-msg for mu4e
  (org-msg-mode-mu4e)
  (org-msg-mode 1))


(use-package mu4e-alert
  :straight t
  :after mu4e
  :config
  (setq doom-modeline-mu4e t
        mu4e-alert-icon "/usr/share/icons/Papirus/64x64/apps/mail-client.svg"
        mu4e-alert-set-window-urgency nil
        mu4e-alert-group-by :to
        mu4e-alert-email-notification-types '(subjects)
        mu4e-alert-interesting-mail-query "flag:unread AND NOT flag:trashed AND NOT maildir:/*junk AND NOT maildir:/*spam")

  (mu4e-alert-enable-mode-line-display)
  (mu4e-alert-enable-notifications)

  (mu4e-alert-set-default-style 'libnotify)

  (defvar +mu4e-alert-bell-cmd
    (when (or sys/linux sys/bsd)
      '("paplay" . "/usr/share/sounds/freedesktop/stereo/message.oga")))

  (defun +mu4e-name-or-email (msg)
    (let* ((from (car (plist-get msg :from)))
           (name (plist-get from :name)))
      (if (or (null name) (eq name ""))
          (plist-get from :email)
        name)))

  (defun +mu4e-alert-grouped-mail-notif-formatter (mail-group _all-mails)
    (when +mu4e-alert-bell-cmd
      (start-process "mu4e-alert-bell" nil (car +mu4e-alert-bell-cmd) (cdr +mu4e-alert-bell-cmd)))
    (let* ((mail-count (length mail-group)))
      (list
       :title (format "You have %d unread email%s"
                      mail-count (if (> mail-count 1) "s" ""))
       :body (concat
              "• "
              (me-str-join
               "\n• "
               (mapcar
                (lambda (msg)
                  (format "<b>%s</b>: %s"
                          (+mu4e-name-or-email msg)
                          (plist-get msg :subject)))
                mail-group))))))

  (setq mu4e-alert-grouped-mail-notification-formatter
        #'+mu4e-alert-grouped-mail-notif-formatter))


(provide 'me-email)
