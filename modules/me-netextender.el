;; -*- lexical-binding: t; -*-


(defvar netextender-process-name "netextender")
(defvar netextender-buffer-name " *NetExtender*")
(defvar netextender-command '("~/.local/bin/netextender"))

;;;###autoload
(defun netextender-start ()
  "Launch a NetExtender VPN session."
  (interactive)
  (unless (get-process netextender-process-name)
    (if (make-process :name netextender-process-name
                      :buffer netextender-buffer-name
                      :command netextender-command)
        (message "Started NetExtender VPN session")
      (message "Cannot start NetExtender"))))


(defun netextender-kill ()
  "Kill the created NetExtender VPN session."
  (interactive)
  (when (get-process netextender-process-name)
    (if (kill-buffer netextender-buffer-name)
        (message "Killed NetExtender VPN session")
      (message "Cannot kill NetExtender"))))


;;;###autoload
(defun netextender-toggle ()
  "Toggle connection to NetExtender."
  (interactive)
  (if (get-process netextender-process-name)
      (netextender-kill)
    (netextender-start)))
