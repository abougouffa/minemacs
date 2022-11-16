;; -*- lexical-binding: t; -*-


(defvar netextender-process-name "netextender")
(defvar netextender-buffer-name " *NetExtender*")
(defvar netextender-command '("~/.local/bin/netextender"))


(defun netextender-check-system ()
  "Return non-nil if system setup is OK."
  (let* ((pppd-command "/usr/sbin/pppd")
         (pppd-modes (file-modes pppd-command)))
    (if (executable-find (car netextender-command))
        ;; pppd must be run as root (via setuid)
        (if (and pppd-modes (zerop (logand (lsh 1 11) pppd-modes))) ;; Check if the setuid bit isn't set
            (prog1 nil ;; return nil
              (message "pppd needs root permissions, please set the setuid bit of %s." pppd-command))
          t)
      (prog1 nil ;; return nil
        (message "The custom NetExtender command %s is not found!" (car netextender-command))))))


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
