;; -*- lexical-binding: t; -*-

(require 'epg)

(defvar ecryptfs-private-dir "Private")

(defvar ecryptfs-root-dir "~/.ecryptfs/")

(defvar ecryptfs-wrapping-independent-p (file-exists-p (concat ecryptfs-root-dir "wrapping-independent")))

(defvar ecryptfs-wrapped-passphrase-file (expand-file-name (concat ecryptfs-root-dir "wrapped-passphrase")))

(defvar ecryptfs-mount-passphrase-sig-file (expand-file-name (concat ecryptfs-root-dir ecryptfs-private-dir ".sig")))

(defvar ecryptfs-buffer-name "*emacs-ecryptfs*")

(defvar ecryptfs-process-name "emacs-ecryptfs")

(defvar ecryptfs--mount-private-cmd "/sbin/mount.ecryptfs_private")

(defvar ecryptfs--umount-private-cmd "/sbin/umount.ecryptfs_private")

(defvar ecryptfs--passphrase
  (lambda ()
    (s-trim-right
     (epg-decrypt-file
      (epg-make-context)
      (expand-file-name (concat ecryptfs-root-dir "my-pass.gpg"))
      nil))))

(defvar ecryptfs-encrypt-filenames-p
  (not (eq 1
           (with-temp-buffer
             (insert-file-contents ecryptfs-mount-passphrase-sig-file)
             (count-lines (point-min) (point-max))))))

(defvar ecryptfs--command-format
  (if ecryptfs-encrypt-filenames-p
      "ecryptfs-insert-wrapped-passphrase-into-keyring %s '%s'"
    "ecryptfs-unwrap-passphrase %s '%s' | ecryptfs-add-passphrase -"))


;;;###autoload
(defun ecryptfs-mount-private ()
  (interactive)
  (unless (and (file-exists-p ecryptfs-wrapped-passphrase-file)
               (file-exists-p ecryptfs-mount-passphrase-sig-file))
    (error "Encrypted private directory \"%s\" is not setup properly."
           ecryptfs-private-dir)
    (return))

  (let ((try-again t))
    (while (and ;; In the first iteration, we try to silently mount the ecryptfs private directory,
            ;; this would succeed if the key is available in the keyring.
            (shell-command ecryptfs--mount-private-cmd
                           ecryptfs-buffer-name)
            try-again)
      (setq try-again nil)
      (message "Encrypted filenames mode [%s]." (if ecryptfs-encrypt-filenames-p "ENABLED" "DISABLED"))
      (shell-command
       (format ecryptfs--command-format
               ecryptfs-wrapped-passphrase-file
               (funcall ecryptfs--passphrase))
       ecryptfs-buffer-name))))


;;;###autoload
(defun ecryptfs-umount-private ()
  (interactive)
  (if (zerop (shell-command ecryptfs--umount-private-cmd
                            ecryptfs-buffer-name))
      (message "Unmounted private directory successfully.")
    (message "Cannot unmount the private directory, seems to be already unmounted.")))
