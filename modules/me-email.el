;;; me-mu4e.el --- Email stuff using mu4e -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")


(add-to-list 'auto-mode-alist '("\\.mailrc\\'" . conf-space-mode))

(defconst +mu4e-load-path "/usr/share/emacs/site-lisp/mu4e/")

(defgroup minemacs-mu4e nil
  "MinEmacs mu4e tweaks."
  :group 'minemacs)

(defconst +mu4e-available-p
  (and (executable-find "mu")
       (executable-find "msmtp")
       (executable-find "mbsync")
       (file-directory-p +mu4e-load-path)))

(use-package mu4e
  :when +mu4e-available-p
  :load-path +mu4e-load-path
  :commands mu4e-compose-new mu4e--start mu4e
  :hook (mu4e-headers-mode . (lambda ()
                               (visual-line-mode -1)
                               (display-line-numbers-mode -1)))
  :init
  (+map! "om" #'mu4e)
  :custom
  (mu4e-confirm-quit t)
  (mu4e-search-results-limit 1000)
  (mu4e-index-cleanup t)
  (mu4e-attachment-dir "~/Downloads/mu4e-attachements/")
  (mu4e-update-interval (* 1 60)) ;; Every 1 min
  (mu4e-context-policy 'pick-first) ;; Start with the first context
  (mu4e-compose-context-policy 'ask) ;; Always ask which context to use when composing a new mail
  (mu4e-sent-messages-behavior 'sent) ;; Will be overwritten for Gmail accounts
  (mu4e-get-mail-command "mbsync -a") ;; Use mbsync to get mails
  (mu4e-index-update-error-warning nil) ;; Do not show warning after update
  (mu4e-hide-index-messages t) ;; Hide minibuffer messages after indexing
  (mu4e-change-filenames-when-moving t)
  (mu4e-completing-read-function #'completing-read) ;; Use `vertico' instead of `ido'
  (sendmail-program (executable-find "msmtp")) ;; Use msmtp to send mails
  (send-mail-function #'smtpmail-send-it)
  (message-sendmail-f-is-evil t)
  (message-sendmail-extra-arguments '("--read-envelope-from"))
  (message-send-mail-function #'message-send-mail-with-sendmail)
  (message-sendmail-envelope-from 'obey-mail-envelope-from)
  (message-mail-user-agent 'mu4e-user-agent)
  (message-kill-buffer-on-exit t) ;; Close after sending
  (mail-envelope-from 'header)
  (mail-specify-envelope-from t)
  (mail-user-agent 'mu4e-user-agent)
  (read-mail-command 'mu4e)
  :config
  (+nvmap! :keymaps 'mu4e-view-mode-map
    "p" #'mu4e-view-save-attachments)
  (+nvmap! :keymaps '(mu4e-headers-mode-map mu4e-view-mode-map)
    "gw" #'+mu4e-open-mail-as-html
    "g RET" #'browse-url-at-point)
  (+map-local! :keymaps '(mu4e-compose-mode-map org-msg-edit-mode-map)
    "s" #'message-send-and-exit
    "d" #'message-kill-buffer
    "S" #'message-dont-send)

  ;; Disable the new `mu4e-modeline-mode', `mu4e-alert' is much nicer.
  (advice-add
   'mu4e :after
   (defun +mu4e--disable-modeline-mode-a (&rest _)
     (mu4e-modeline-mode -1)))

  ;; No need to display a long list of my own addresses!
  (setq mu4e-main-hide-personal-addresses t)

  (defun +mu4e-open-mail-as-html ()
    "Open the HTML mail in EAF Browser."
    (interactive)
    (if-let ((msg (mu4e-message-at-point t))
             ;; Bind browse-url-browser-function locally, so it works
             ;; even if EAF Browser is not set as a default browser.
             (browse-url-browser-function
              (cond
               ((featurep 'me-eaf) #'eaf-open-browser)
               (t #'browse-url-xdg-open))))
        (mu4e-action-view-in-browser msg)
      (message "No message at point.")))

  ;; Force running update and index in background
  (advice-add
   'mu4e-update-mail-and-index :around
   (defun +mu4e--update-mail-quitely-a (origfn run-in-background)
     (+info! "Getting new emails")
     (apply origfn '(t)))))

;; Reply to iCalendar meeting requests
(use-package mu4e-icalendar
  :when +mu4e-available-p
  :load-path +mu4e-load-path
  :after mu4e
  :demand t
  :config
  (mu4e-icalendar-setup))

(use-package me-mu4e-ui
  :after mu4e
  :demand t
  :config
  ;; Setup the UI (mostly inspired by Doom Emacs, with a lot of improvements)
  (+mu4e-ui-setup))

(use-package me-mu4e-gmail
  :after mu4e
  :demand t
  :config
  ;; Setup Gmail specific hacks (adapted from Doom Emacs, with a lot of
  ;; improvements)
  (+mu4e-gmail-setup))

(use-package me-mu4e-extras
  :after mu4e
  :demand t
  :config
  ;; Enable MinEmacs's mu4e extra features, including:
  ;; - Auto BCC the `+mu4e-auto-bcc-address';
  ;; - Prompt for the "From" address from the account aliases `+mu4e-account-aliases';
  ;; - Check for the subject before sending;
  ;; - Add an action to save the mail as PDF;
  ;; - Add an action to save all the attachements;
  ;; - Add an action to save the message at point.
  (+mu4e-extras-setup)
  ;; Redefine bookmarks queries to ignore spams
  (+mu4e-extras-ignore-spams-in-bookmarks-setup))

(use-package org-msg
  :straight t
  :after mu4e
  :demand t
  :custom
  (org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil tex:dvipng")
  (org-msg-startup "hidestars indent inlineimages")
  (org-msg-greeting-name-limit 3)
  (org-msg-convert-citation t)
  (org-msg-default-alternatives '((new . (utf-8 html))
                                  (reply-to-text . (utf-8 html))
                                  (reply-to-html . (utf-8 html))))
  (org-msg-attached-file-reference
   (rx (or (seq "attach" (or "ment" "ed"))
           (seq "enclosed")
           (seq "attach" (any ?é ?e) (? "e") (? "s"))
           (seq "ci" (or " " "-") "joint" (? "e")) ;; ci-joint
           (seq (or (seq "pi" (any ?è ?e) "ce") "fichier" "document") (? "s") (+ (or " " eol)) "joint" (? "e") (? "s")) ;; pièce jointe
           (seq (or (seq space "p" (zero-or-one (any ?- ?.)) "j" space)))))) ;; p.j
  :config
  (+nvmap! :keymaps 'org-msg-edit-mode-map
    "TAB" #'org-msg-tab
    "gg"  #'org-msg-goto-body)
  (+map-local! :keymaps 'org-msg-edit-mode-map
    "a"  '(nil :wk "attach")
    "aa" #'org-msg-attach-attach
    "ad" #'org-msg-attach-delete
    "p"  #'org-msg-preview)
  (org-msg-mode 1))

(use-package mu4e-alert
  :straight t
  :after mu4e
  :demand t
  :custom
  (mu4e-alert-icon "/usr/share/icons/Papirus/64x64/apps/mail-client.svg")
  (mu4e-alert-set-window-urgency nil)
  (mu4e-alert-group-by :to)
  (mu4e-alert-email-notification-types '(subjects))
  :init
  (defcustom +mu4e-alert-bell-command
    (when (or os/linux os/bsd)
      '("paplay" . "/usr/share/sounds/freedesktop/stereo/message.oga"))
    "A cons list of the command and arguments to play the notification bell."
    :group 'minemacs-mu4e
    :type '(cons string string))
  :config
  ;; Enable on mu4e notifications in doom-modeline
  (setq doom-modeline-mu4e t)

  ;; Ignore spams!
  (setq mu4e-alert-interesting-mail-query
        (+mu4e-extras-ignore-spams-query mu4e-alert-interesting-mail-query))

  (mu4e-alert-enable-mode-line-display)
  (mu4e-alert-enable-notifications)
  (mu4e-alert-set-default-style 'libnotify)

  (defun +mu4e-name-or-email (msg)
    (let* ((from (car (plist-get msg :from)))
           (name (plist-get from :name)))
      (if (or (null name) (eq name ""))
          (plist-get from :email)
        name)))

  (defun +mu4e-alert-grouped-mail-notif-formatter (mail-group _all-mails)
    (when +mu4e-alert-bell-command
      (start-process "mu4e-alert-bell" nil (car +mu4e-alert-bell-command) (cdr +mu4e-alert-bell-command)))
    (let ((mail-count (length mail-group)))
      (list
       :title (format "You have %d unread email%s"
                      mail-count (if (> mail-count 1) "s" ""))
       :body (concat
              "• "
              (string-join
               (mapcar
                (lambda (msg)
                  (format "<b>%s</b>: %s"
                          (+mu4e-name-or-email msg)
                          (plist-get msg :subject)))
                mail-group)
               "\n• ")))))

  (setq mu4e-alert-grouped-mail-notification-formatter
        #'+mu4e-alert-grouped-mail-notif-formatter))


(provide 'me-email)
