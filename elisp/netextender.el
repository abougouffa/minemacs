;; -*- lexical-binding: t; -*-


(defvar netextender-process-name "netextender")
(defvar netextender-buffer-name " *NetExtender*")
(defvar netextender-command '("~/.local/bin/netextender"))
(defvar netextender-passphrase-file "~/.ssh/sslvpn.gpg")

;; If the command doesn't exist, generate it.
(unless (file-exists-p (car netextender-command))
  (setq netextender-command (cons (make-temp-file "netextender") (cdr netextender-command)))
  (set-file-modes (car netextender-command) #o755) ;; Make it executable
  (with-temp-buffer
    (insert
     (format "#!/bin/bash

if ! command -v netExtender &> /dev/null; then
  echo \"netExtender not found, installing from AUR using 'yay'\"
  yay -S netextender
fi

MY_LOGIN_PARAMS_FILE=\"%s\"

echo \"Y\\n\" | netExtender --auto-reconnect $(gpg -q --for-your-eyes-only --no-tty -d \"${MY_LOGIN_PARAMS_FILE}\")"
             (expand-file-name netextender-passphrase-file)))
    (write-file (car netextender-command))))

(defun netextender-check-system ()
  "Return non-nil if system setup is OK."
  (let* ((pppd-command "/usr/sbin/pppd")
         (pppd-modes (file-modes pppd-command)))
    ;; pppd must be run as root (via setuid)
    (if (and pppd-modes (zerop (logand (lsh 1 11) pppd-modes))) ;; Check if the setuid bit isn't set
        (prog1 nil ;; return nil
          (message "pppd needs root permissions, please set the setuid bit of %s." pppd-command))
      t)))

;;;###autoload
(defun netextender-start ()
  "Launch a NetExtender VPN session."
  (interactive)
  (if (netextender-check-system)
      (unless (get-process netextender-process-name)
        (if (make-process :name netextender-process-name
                          :buffer netextender-buffer-name
                          :command netextender-command)
            (message "Started NetExtender VPN session.")
          (user-error "Cannot start NetExtender.")))
    (user-error "Cannot start a netExtender VPN session.")))

(defun netextender-kill ()
  "Kill the created NetExtender VPN session."
  (interactive)
  (let ((netextender-process (get-process netextender-process-name)))
    (if netextender-process
        (if (kill-process netextender-process)
            (message "Killed NetExtender VPN session.")
          (user-error "Cannot kill NetExtender."))
      (message "No running NetExtender session."))))

;;;###autoload
(defun netextender-toggle ()
  "Toggle connection to NetExtender."
  (interactive)
  (if (get-process netextender-process-name)
      (netextender-kill)
    (netextender-start)))
