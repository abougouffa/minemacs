;; -*- lexical-binding: t; -*-

(defconst MU4E-LOAD-PATH "/usr/share/emacs/site-lisp/mu4e/")

(use-package mu4e
  :when (file-directory-p MU4E-LOAD-PATH)
  :load-path MU4E-LOAD-PATH
  :init
  ;; (provide 'html2text) ; disable obsolete package
  (setq mu4e-attachment-dir (expand-file-name "~/Downloads/mu4e-attachements"))
  :general
  (me-global-def "om" '(mu4e :which-key "Mu4e"))
  :config
  (load (expand-file-name "lisp/mu4e-extra-stuff.el" user-emacs-directory))
  ;; (me-local-def :keymaps mu4e-compose-mode-map
  ;;   "s" #'message-send-and-exit
  ;;   "d" #'message-kill-buffer
  ;;   "S" #'message-dont-send
  ;;   "a" #'+mu4e/attach-files)
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
        mu4e-update-interval (* 3 60) ;; Every 3 min
        mu4e-context-policy 'pick-first ;; Start with the first context
        mu4e-compose-context-policy 'ask ;; Always ask which context to use when composing a new mail
        mu4e-sent-messages-behavior 'sent
        mu4e-get-mail-command "mbsync -a" ;; Not needed, as +mu4e-backend is 'mbsync by default
        mu4e-index-update-error-warning nil ;; Do not show warning after update
        mu4e-main-hide-personal-addresses t ;; No need to display a long list of my own addresses!
        mu4e-change-filenames-when-moving t
        mu4e-completing-read-function (if (featurep 'vertico) #'completing-read #'ido-completing-read))

  (setq sendmail-program (executable-find "msmtp")
        send-mail-function #'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail
        message-sendmail-envelope-from 'obey-mail-envelope-from
        message-kill-buffer-on-exit t ; close after sending
        mail-envelope-from 'header
        mail-specify-envelope-from t
        mail-user-agent 'mu4e-user-agent
        message-mail-user-agent 'mu4e-user-agent)

  ;; Due to evil, none of the marking commands work when making a visual selection in
  ;; the headers view of mu4e. Without overriding any evil commands we may actually
  ;; want to use in and evil selection, this can be easily fixed.
  ;; (when (modulep! :editor evil)
  ;;   (map! :map mu4e-headers-mode-map
  ;;         :v "*" #'mu4e-headers-mark-for-something
  ;;         :v "!" #'mu4e-headers-mark-for-read
  ;;         :v "?" #'mu4e-headers-mark-for-unread
  ;;         :v "u" #'mu4e-headers-mark-for-unmark))

  ;; (add-hook 'mu4e-compose-pre-hook '+mu4e-set-from-address-h)

  ;;   (defadvice! +mu4e-ensure-compose-writeable-a (&rest _)
  ;;     "Ensure that compose buffers are writable.
  ;; This should already be the case yet it does not always seem to be."
  ;;     :before #'mu4e-compose-new
  ;;     :before #'mu4e-compose-reply
  ;;     :before #'mu4e-compose-forward
  ;;     :before #'mu4e-compose-resend
  ;;     (read-only-mode -1))

  ;;(add-hook 'kill-emacs-hook #'+mu4e-lock-file-delete-maybe)
  ;;(advice-add 'mu4e--start :around #'+mu4e-lock-start)
  ;;(advice-add 'mu4e-quit :after #'+mu4e-lock-file-delete-maybe))

  (defun me-mu4e--ui-setup ()
    (setq mu4e-use-fancy-chars t
          mu4e-headers-draft-mark      (cons "D" (all-the-icons-material "edit"))
          mu4e-headers-flagged-mark    (cons "F" (all-the-icons-material "flag"))
          mu4e-headers-new-mark        (cons "N" (all-the-icons-material "file_download" :color "dred"))
          mu4e-headers-passed-mark     (cons "P" (all-the-icons-material "forward"))
          mu4e-headers-replied-mark    (cons "R" (all-the-icons-material "reply"))
          mu4e-headers-seen-mark       (cons "S" "")
          mu4e-headers-trashed-mark    (cons "T" (all-the-icons-material "delete"))
          mu4e-headers-attach-mark     (cons "a" (all-the-icons-material "attach_file"))
          mu4e-headers-encrypted-mark  (cons "x" (all-the-icons-material "lock"))
          mu4e-headers-signed-mark     (cons "s" (all-the-icons-material "verified_user" :color "dpurple"))
          mu4e-headers-unread-mark     (cons "u" (all-the-icons-material "remove_red_eye" :color "dred"))
          mu4e-headers-list-mark       (cons "l" (all-the-icons-material "list"))
          mu4e-headers-personal-mark   (cons "p" (all-the-icons-material "person"))
          mu4e-headers-calendar-mark   (cons "c" (all-the-icons-material "date_range"))
          mu4e-headers-fields '((:flags . 6) ;; 3 flags
                                (:account-stripe . 2)
                                (:from-or-to . 25)
                                (:folder . 10)
                                (:recipnum . 2)
                                (:subject . 80)
                                (:human-date . 8))
          mu4e-headers-date-format "%d/%m/%y"
          mu4e-headers-time-format "⧖ %H:%M"
          mu4e-headers-thread-single-orphan-prefix '("─>" . "─▶")
          mu4e-headers-thread-orphan-prefix        '("┬>" . "┬▶ ")
          mu4e-headers-thread-connection-prefix    '("│ " . "│ ")
          mu4e-headers-thread-first-child-prefix   '("├>" . "├▶")
          mu4e-headers-thread-child-prefix         '("├>" . "├▶")
          mu4e-headers-thread-last-child-prefix    '("└>" . "╰▶")
          ;; remove 'lists' column
          mu4e-headers-fields
          '((:account-stripe . 1)
            (:human-date . 12)
            (:flags . 6) ; 3 icon flags
            (:from-or-to . 25)
            (:subject)))
    ;; mu4e-headers-threaded-label
    ;; (make-help-button-cons "T" (concat " " (all-the-icons-octicon "git-branch" :v-adjust 0.05))
    ;;                        "Thread view")
    ;; mu4e-headers-related-label
    ;; (make-help-button-cons "R" (concat " " (all-the-icons-material "link" :v-adjust -0.1))
    ;;                        "Showing related emails")
    ;; mu4e-headers-full-label
    ;; (make-help-button-cons "F" (concat " " (all-the-icons-material "disc_full"))
    ;;                        "Search is full!"))

    (defvar +mu4e-main-bullet "⚫"
      "Prefix to use instead of \"  *\" in the mu4e main view.
This is enacted by `+mu4e~main-action-str-prettier-a' and
`+mu4e~main-keyval-str-prettier-a'."))

  (me-mu4e--ui-setup))

;; (advice-add #'mu4e--key-val :filter-return #'+mu4e~main-keyval-str-prettier-a)
;; (advice-add #'mu4e--main-action-str :override #'+mu4e~main-action-str-prettier-a)
;; (when (modulep! :editor evil)
;;   ;; As +mu4e~main-action-str-prettier replaces [k]ey with key q]uit should become quit
;;   (setq evil-collection-mu4e-end-region-misc "quit"))
;; (unless (modulep! +org)
;;   (after! mu4e
;;           (defun org-msg-mode (&optional _)
;;             "Dummy function."
;;             (message "Enable the +org mu4e flag to use org-msg-mode."))
;;           (defun +mu4e-compose-org-msg-handle-toggle (&rest _)
;;             "Placeholder to allow for the assumtion that this function is defined.
;; Ignores all arguments and returns nil."
;;             nil)))


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
