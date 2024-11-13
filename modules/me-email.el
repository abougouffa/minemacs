;;; me-mu4e.el --- Email stuff using mu4e -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(add-to-list 'auto-mode-alist '("\\.mailrc\\'" . conf-space-mode))

(defconst +mu4e-load-path "/usr/share/emacs/site-lisp/mu4e/")

(defgroup minemacs-mu4e nil
  "MinEmacs mu4e tweaks."
  :group 'minemacs-apps)

(defconst +mu4e-available-p
  (and (executable-find "mu") (executable-find "msmtp") (executable-find "mbsync") (file-directory-p +mu4e-load-path)))


;; Emacs Email agent based on the "mu" indexer
(use-package mu4e
  :when +mu4e-available-p
  :load-path +mu4e-load-path
  :commands (mu4e-compose-new mu4e)
  :autoload mu4e--start
  :hook (mu4e-headers-mode . (lambda ()
                               (visual-line-mode -1)
                               (display-line-numbers-mode -1)))
  :bind (:map minemacs-open-thing-map ("m" . +mu4e))
  :custom-face
  (mu4e-thread-fold-face ((t (:inherit default))))
  :init
  (defcustom +mu4e-auto-start t
    "Automatically start `mu4e' in background in `me-daemon'."
    :group 'minemacs-mu4e
    :type 'boolean)
  ;; Make `+mu4e' dedicated tab for `mu4e'
  (+def-dedicated-tab! mu4e :exit-func mu4e-quit)
  :custom
  (mu4e-confirm-quit t)
  (mu4e-search-results-limit 1000)
  (mu4e-index-cleanup t)
  (mu4e-attachment-dir "~/Downloads/mu4e-attachements/")
  (mu4e-update-interval (* 1 60)) ; Every 1 min
  (mu4e-context-policy 'pick-first) ; Start with the first context
  (mu4e-compose-context-policy 'ask) ; Always ask which context to use when composing a new mail
  (mu4e-sent-messages-behavior 'sent) ; Will be overwritten for Gmail accounts
  (mu4e-get-mail-command "mbsync -a") ; Use mbsync to get mails
  (mu4e-index-update-error-warning nil) ; Do not show warning after update
  (mu4e-hide-index-messages t) ; Hide minibuffer messages after indexing
  (mu4e-change-filenames-when-moving t)
  (mu4e-completing-read-function #'completing-read) ; Use `vertico' instead of `ido'
  (mu4e-main-hide-personal-addresses t) ; Don't display a list of my own addresses!
  (mu4e-modeline-support nil) ; `mu4e-alert' is much nicer.
  (read-mail-command 'mu4e)
  (message-kill-buffer-on-exit t) ; Close after sending
  (message-mail-user-agent 'mu4e-user-agent)
  (sendmail-program (executable-find "msmtp")) ; Use msmtp to send mails
  (send-mail-function #'smtpmail-send-it)
  (message-sendmail-f-is-evil t)
  (message-sendmail-extra-arguments '("--read-envelope-from"))
  (message-send-mail-function #'message-send-mail-with-sendmail)
  (message-sendmail-envelope-from 'obey-mail-envelope-from)
  (mail-envelope-from 'header)
  (mail-specify-envelope-from t)
  (mail-user-agent 'mu4e-user-agent)
  :config
  (defun +mu4e-open-mail-as-html ()
    "Open the HTML mail in EAF Browser."
    (interactive)
    (if-let* ((msg (mu4e-message-at-point t))
              (browse-url-browser-function
               (cond
                ((featurep 'xwidget-webkit) #'xwidget-webkit-browse-url)
                (t #'browse-url-xdg-open))))
        (mu4e-action-view-in-browser msg)
      (message "No message at point.")))

  ;; Force running update and index in background
  (advice-add
   'mu4e-update-mail-and-index :around
   (satch-defun +mu4e--update-mail-quitely:around-a (origfn _run-in-background)
     (+info! "Getting new emails")
     (apply origfn '(t)))))


;; Reply to iCalendar meeting requests
(use-package mu4e-icalendar
  :when +mu4e-available-p
  :load-path +mu4e-load-path
  :after mu4e
  :demand
  :custom
  (mu4e-icalendar-trash-after-reply t)
  :config
  (gnus-icalendar-setup))


;; Send ical calendar invites by email
(use-package varuga
  :straight t
  :commands (varuga-invite))


;; My UI tweaks for `mu4e'
(use-package me-mu4e-ui
  :when +mu4e-available-p
  :after mu4e
  :demand
  :config
  ;; Setup the UI (mostly inspired by Doom Emacs, with a lot of improvements)
  (+mu4e-ui-setup)
  (+mu4e-ui-modeline-tweaks))

(use-package me-mu4e-gmail
  :when +mu4e-available-p
  :after mu4e
  :demand
  :config
  ;; Setup Gmail specific hacks (adapted from Doom Emacs, with a lot of improvements)
  (+mu4e-gmail-setup))

(use-package me-mu4e-extras
  :when +mu4e-available-p
  :after mu4e
  :demand
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
  (+mu4e-extras-ignore-spams-in-bookmarks-setup)
  ;; Create/destroy lock files on server start/kill
  (+mu4e-extras-locks-setup))


;; Global minor mode mixing up `org-mode' and `message-mode' to compose and reply to emails in a Outlook HTML friendly style
(use-package org-msg
  :straight t
  :when +mu4e-available-p
  :after mu4e
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
           (seq (or (seq space "p" (zero-or-one (any ?- ?.)) "j" (any space ?: ?\; ?, ?.))))))) ;; p.j
  :init
  (org-msg-mode 1)

  ;; HACK: When adding multiple attachments, I likely need it to remember the directory of the last added attachment.
  (advice-add
   'org-msg-attach-attach :after
   (satch-defun +org-msg-attach-attach--save-default-directory:after-a (file &rest _)
     (when-let* ((dir (file-name-directory file)))
       (setq-local default-directory dir)))))


;; Send HTML email using Org-mode HTML export (alternative to `org-msg')
(use-package org-mime
  :straight t
  :when +mu4e-available-p
  :after mu4e org
  :demand
  :config
  ;; Do not export table of contents nor author name
  (setq org-mime-export-options '(:with-latex dvipng :section-numbers t :with-author nil :with-toc nil)))


;; Desktop notifications and modeline display for `mu4e'
(use-package mu4e-alert
  :straight t
  :when +mu4e-available-p
  :after mu4e
  :demand
  :custom
  (mu4e-alert-icon
   (let ((icon "/usr/share/icons/Papirus/64x64/apps/mail-client.svg"))
     (when (file-exists-p icon) icon)))
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
  ;; Enable `mu4e' segment in `doom-modeline'
  (setq doom-modeline-mu4e t)

  ;; Ignore spams!
  (setq mu4e-alert-interesting-mail-query (+mu4e-extras-ignore-spams-query mu4e-alert-interesting-mail-query))

  (mu4e-alert-enable-mode-line-display)
  (mu4e-alert-enable-notifications)
  (mu4e-alert-set-default-style 'libnotify)

  (defun +mu4e-name-or-email (msg)
    (let* ((from (car (plist-get msg :from)))
           (name (plist-get from :name)))
      (if (or (null name) (eq name "")) (plist-get from :email) name)))

  (defun +mu4e-alert-grouped-mail-notif-formatter (mail-group _all-mails)
    "This function can be used for `mu4e-alert-grouped-mail-notification-formatter'."
    (when +mu4e-alert-bell-command
      (start-process "mu4e-alert-bell" nil (car +mu4e-alert-bell-command) (cdr +mu4e-alert-bell-command)))
    (let ((mail-count (length mail-group)))
      (list
       :title (format "You have %d unread email%s" mail-count (if (> mail-count 1) "s" ""))
       :body (concat
              "• "
              (string-join
               (mapcar
                (lambda (msg)
                  (format "<b>%s</b>: %s" (+mu4e-name-or-email msg) (plist-get msg :subject)))
                mail-group)
               "\n• ")))))

  (setq mu4e-alert-grouped-mail-notification-formatter #'+mu4e-alert-grouped-mail-notif-formatter))


;; Encrypt and decrypt mails in `mu4e'
(use-package mu4e-crypto
  :straight t
  :when +mu4e-available-p
  :after mu4e
  :bind (:map mu4e-compose-mode-map ("C-c C-m e" . mu4e-crypto-encrypt-message))
  :bind (:map mu4e-view-mode-map ("C-c C-m d" . mu4e-crypto-decrypt-message)))


(provide 'me-email)

;;; me-email.el ends here
