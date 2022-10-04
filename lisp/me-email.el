;; -*- lexical-binding: t; -*-

(defconst MU4E-LOAD-PATH "/usr/share/emacs/site-lisp/mu4e/")

(use-package mu4e
  :when (file-directory-p MU4E-LOAD-PATH)
  :load-path MU4E-LOAD-PATH
  :commands mu4e mu4e-compose-new mu4e--start
  :general
  (me-global-def "om" '(mu4e :which-key "Mu4e"))
  :config
  (require 'me-mu4e-extras)

  (me-local-def :keymaps 'mu4e-compose-mode-map
    "s" #'message-send-and-exit
    "d" #'message-kill-buffer
    "S" #'message-dont-send)
  (me-map-def :keymaps 'mu4e-view-mode-map
    "A" #'+mu4e-view-select-mime-part-action
    "p" #'mu4e-view-save-attachments
    "P" #'+mu4e-view-save-all-attachments
    "o" #'+mu4e-view-open-attachment)

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

  ;; Don't save message to Sent, Gmail's IMAP takes care of this.
  (setq mu4e-sent-messages-behavior
        (lambda ()
          (if (or (string-match-p "@gmail.com\\'" (message-sendmail-envelope-from))
                  (member (message-sendmail-envelope-from)
                          (mapcar #'car +mu4e-gmail-accounts)))
              'delete 'sent)))

  ;; Html mails might be better rendered in a browser
  (add-to-list 'mu4e-view-actions '("View in browser" . mu4e-action-view-in-browser))
  (when (fboundp 'make-xwidget)
    (add-to-list 'mu4e-view-actions '("View in xwidgets" . mu4e-action-view-in-xwidget)))

  (add-hook 'message-send-hook #'+mu4e-check-for-subject))


(use-package org-msg
  :straight t
  :after mu4e org
  :general
  (me-map-def :keymaps 'org-msg-edit-mode-map
    "TAB" '(org-msg-tab :which-key "org-msg-tab"))
  :config
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
                (seq "ci" (or " " "-") "joint" (? "e"))
                (seq (or (seq "pi" (any ?è ?e) "ce") "fichier" "document") (? "s") (+ (or " " eol)) "joint" (? "e") (? "s")))))

  ;; Setup Org-msg for mu4e
  (org-msg-mode-mu4e))


(use-package mu4e-alert
  :straight t
  :after mu4e
  :config
  (setq doom-modeline-mu4e t
        mu4e-alert-set-window-urgency nil
        mu4e-alert-email-notification-types '(subjects)
        mu4e-alert-icon "/usr/share/icons/Papirus/64x64/apps/mail-client.svg")

  (defvar +mu4e-alert-bell-cmd '("paplay" . "/usr/share/sounds/freedesktop/stereo/message.oga"))
  (mu4e-alert-set-default-style 'libnotify)

  (defun +mu4e-alert-helper-name-or-email (msg)
    (let* ((from (car (plist-get msg :from)))
           (name (plist-get from :name)))
      (if (or (null name) (eq name ""))
          (plist-get from :email)
        name)))

  (defun +mu4e-alert-grouped-mail-notif-formatter (mail-group _all-mails)
    (when +mu4e-alert-bell-cmd
      (start-process "mu4e-alert-bell" nil (car +mu4e-alert-bell-cmd) (cdr +mu4e-alert-bell-cmd)))
    (let* ((filtered-mails (me-filter
                            (lambda (msg)
                              (not (string-match-p "\\(junk\\|spam\\|trash\\|deleted\\)"
                                                   (downcase (plist-get msg :maildir)))))
                            mail-group))
           (mail-count (length filtered-mails)))
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
                          (+mu4e-alert-helper-name-or-email msg)
                          (plist-get msg :subject)))
                filtered-mails))))))

  (setq mu4e-alert-grouped-mail-notification-formatter
        #'+mu4e-alert-grouped-mail-notif-formatter)

  (mu4e-alert-enable-mode-line-display)
  (mu4e-alert-enable-notifications))


(use-package evil-mu4e
  :disabled t
  :straight t
  :after mu4e)


(provide 'me-email)
