;; -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;; This allows me to mount my private directory encrypted using ecryptfs-utils.
;; It is a port of "ecryptfs-mount-private" shell command. It uses extracts the
;; encryption key from a GPG encrypted file containting the ecryptfs password.
;; The decryption of the password is performed using Emacs' `epg'.

(require 'epg)

(defgroup minemacs-ecryptfs nil
  "MinEmacs eCryptfs."
  :group 'minemacs)

(defcustom ecryptfs-private-dir-name "Private"
  "eCryptfs private directory name."
  :group 'minemacs-ecryptfs
  :type 'string)

(defcustom ecryptfs-root-dir "~/.ecryptfs/"
  "eCryptfs root configuration directory."
  :group 'minemacs-ecryptfs
  :type 'directory)

(defvar ecryptfs-wrapping-independent-p (file-exists-p (concat ecryptfs-root-dir "wrapping-independent")))

(defvar ecryptfs-wrapped-passphrase-file (concat ecryptfs-root-dir "wrapped-passphrase"))

(defvar ecryptfs-mount-passphrase-sig-file (concat ecryptfs-root-dir ecryptfs-private-dir-name ".sig"))

(defvar ecryptfs-buffer-name "*emacs-ecryptfs*")

(defvar ecryptfs-process-name "emacs-ecryptfs")

(defvar ecryptfs--mount-private-cmd "/sbin/mount.ecryptfs_private")

(defvar ecryptfs--umount-private-cmd "/sbin/umount.ecryptfs_private")

(defvar ecryptfs--passphrase
  (lambda ()
    (string-trim-right
     (epg-decrypt-file
      (epg-make-context)
      (expand-file-name (concat ecryptfs-root-dir "my-pass.gpg"))
      nil))))

(defvar ecryptfs-encrypt-filenames-p
  (not (eq 1 (with-temp-buffer
               (insert-file-contents ecryptfs-mount-passphrase-sig-file)
               (count-lines (point-min) (point-max))))))

(defvar ecryptfs--command-format
  (if ecryptfs-encrypt-filenames-p
      "ecryptfs-insert-wrapped-passphrase-into-keyring %s '%s'"
    "ecryptfs-unwrap-passphrase %s '%s' | ecryptfs-add-passphrase -"))

(defun ecryptfs-private-mounted-p ()
  (let ((mount (shell-command-to-string "mount")))
    (and (string-match-p (concat ".*" (expand-file-name ecryptfs-private-dir-name "~") ".*ecryptfs.*") mount)
         t)))

;;;###autoload
(defun ecryptfs-toggle-mount-private ()
  "Mount/Unmount eCryptfs' private directory."
  (interactive)
  (if (ecryptfs-private-mounted-p)
      (ecryptfs-umount-private)
    (ecryptfs-mount-private)))

;;;###autoload
(defun ecryptfs-mount-private ()
  "Mount eCryptfs' private directory."
  (interactive)
  (if (not (and (file-exists-p ecryptfs-wrapped-passphrase-file)
                (file-exists-p ecryptfs-mount-passphrase-sig-file)))
      (user-error "Encrypted private directory \"%s\" is not setup properly."
                  ecryptfs-private-dir-name)
    (let ((try-again t))
      (message "Encrypted filenames mode [%s]" (if ecryptfs-encrypt-filenames-p "✓" "⨯"))
      (while (and ;; In the first iteration, we try to silently mount the ecryptfs private directory,
              ;; this would succeed if the key is available in the keyring.
              (and (shell-command ecryptfs--mount-private-cmd
                                  ecryptfs-buffer-name)
                   (message "Successfully mounted private directory."))
              try-again)
        (setq try-again nil)
        (if (zerop
             (shell-command
              (format ecryptfs--command-format
                      ecryptfs-wrapped-passphrase-file
                      (funcall ecryptfs--passphrase))
              ecryptfs-buffer-name))
            (message "Successfully mounted private directory.")
          (user-error "A problem occured while mounting the private directory, see %s"
                      ecryptfs-buffer-name))))))

;;;###autoload
(defun ecryptfs-umount-private ()
  "Unmount eCryptfs' private directory."
  (interactive)
  (if (zerop (shell-command ecryptfs--umount-private-cmd
                            ecryptfs-buffer-name))
      (message "Unmounted private directory successfully.")
    (user-error "Cannot unmount the private directory, seems to be already unmounted.")))
