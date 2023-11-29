;; -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(defvar netextender-process-name "netextender")
(defvar netextender-buffer-name " *NetExtender*")

(defgroup minemacs-netextender nil
  "MinEmacs NetExtender."
  :group 'minemacs)

(defcustom netextender-passphrase-file "~/.ssh/sslvpn.gpg"
  "GPG encrypted file containing NetExtender connection parameters.

The file contains this line:
\"-u <USERNAME> -d <DOMAIN> -p <PASSWORD> -s <SERVER>\"

It includes the connections credentials (username and password), hence, it is
mandatory stored as a GPG encrypted file."
  :group 'minemacs-netextender
  :type 'file)

(defcustom netextender-command "netExtender"
  "The NetExtender CLI command."
  :group 'minemacs-netextender
  :type '(choice string file))

(defcustom netextender-launcher-command (locate-user-emacs-file "netextender-launcher.sh")
  "Custom NetExtender launcher command.

This is a wrapper around the \"NetExtender\" command. It starts the sessions
without asking about connection credentials.

If this command doesn't exist, it will be created automatically (you need to set
`netextender-passphrase-file' accordingly)."
  :group 'minemacs-netextender
  :type '(choice string file))

(defun netextender-launcher-command ()
  "Get the NetExtender launcher command.

Returns `netextender-launcher-command' if it exists, otherwise, it creates a
temporary based on `netextender-command' and `netextender-passphrase-file' and
returns it."
  ;; If the command doesn't exist, generate it.
  (if (not (executable-find netextender-command))
      (user-error "The NetExtender command \"%s\" is not available" netextender-command)
    (unless (executable-find netextender-launcher-command)
      (setq netextender-launcher-command (make-temp-file "netextender-launcher-" nil ".sh"))
      (set-file-modes netextender-launcher-command #o755) ;; Make it executable
      (with-temp-buffer (insert (format "#!/bin/bash

MY_LOGIN_PARAMS_FILE=\"%s\"

echo \"Y\\n\" | %s --auto-reconnect $(gpg -q --for-your-eyes-only --no-tty -d \"${MY_LOGIN_PARAMS_FILE}\")"
                                        (expand-file-name netextender-passphrase-file)
                                        (executable-find netextender-command)))
                        (write-file netextender-launcher-command)))
    ;; Return the command
    netextender-launcher-command))

(defun netextender-system-ok-p ()
  "Return non-nil if system setup is OK."
  (let* ((pppd-command "/usr/sbin/pppd")
         (pppd-modes (file-modes pppd-command)))
    ;; pppd must be run as root (via setuid)
    (if (and pppd-modes (zerop (logand (lsh 1 11) pppd-modes))) ;; Check if the setuid bit isn't set
        (prog1 nil ;; return nil
          (user-error "The `pppd' command needs root permissions, please set the setuid bit of %s" pppd-command))
      t)))

;;;###autoload
(defun netextender-start ()
  "Launch a NetExtender VPN session."
  (interactive)
  (if (netextender-system-ok-p)
      (unless (get-process netextender-process-name)
        (if (make-process :name netextender-process-name
                          :buffer netextender-buffer-name
                          :command (list (netextender-launcher-command)))
            (message "Started NetExtender VPN session.")
          (user-error "Cannot start NetExtender")))
    (user-error "Cannot start a netExtender VPN session")))

(defun netextender-kill ()
  "Kill the created NetExtender VPN session."
  (interactive)
  (let ((netextender-process (get-process netextender-process-name)))
    (if netextender-process
        (if (kill-process netextender-process)
            (message "Killed NetExtender VPN session.")
          (user-error "Cannot kill NetExtender"))
      (message "No running NetExtender session."))))

;;;###autoload
(defun netextender-toggle ()
  "Toggle connection to NetExtender."
  (interactive)
  (if (get-process netextender-process-name)
      (netextender-kill)
    (netextender-start)))


(provide 'netextender)

;;; netextender.el ends here
