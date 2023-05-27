;; -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;; This allows me to mount my private directory encrypted using ecryptfs-utils.
;; It is a port of "ecryptfs-mount-private" shell command. It uses extracts the
;; encryption key from a GPG encrypted file containting the ecryptfs password.
;; The decryption of the password is performed using Emacs' `epg'.

;;; Code:

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

(defvar ecryptfs-buffer-name " *emacs-ecryptfs*")
(defvar ecryptfs-process-name "emacs-ecryptfs")
(defvar ecryptfs--mount-private-cmd "/sbin/mount.ecryptfs_private")
(defvar ecryptfs--umount-private-cmd "/sbin/umount.ecryptfs_private")

(defun ecryptfs--wrapped-passphrase-file ()
  (concat ecryptfs-root-dir "wrapped-passphrase"))

(defun ecryptfs--mount-passphrase-sig-file ()
  (concat ecryptfs-root-dir ecryptfs-private-dir-name ".sig"))

(defun ecryptfs--passphrase ()
  (string-trim-right
   (epg-decrypt-file
    (epg-make-context)
    (expand-file-name (concat ecryptfs-root-dir "my-pass.gpg"))
    nil)))

(defun ecryptfs--encrypt-filenames-p ()
  (/= 1 (with-temp-buffer
          (insert-file-contents (ecryptfs--mount-passphrase-sig-file))
          (count-lines (point-min) (point-max)))))

(defun ecryptfs-available-p ()
  (and (file-directory-p (expand-file-name ecryptfs-private-dir-name "~"))
       (cl-every #'file-exists-p (list ecryptfs--mount-private-cmd
                                       ecryptfs--umount-private-cmd
                                       (ecryptfs--wrapped-passphrase-file)
                                       (ecryptfs--mount-passphrase-sig-file)))))

(defun ecryptfs--unwrap-passphrase-command ()
  (format
   (if (ecryptfs--encrypt-filenames-p)
       "ecryptfs-insert-wrapped-passphrase-into-keyring %s '%s'"
     "ecryptfs-unwrap-passphrase %s '%s' | ecryptfs-add-passphrase -")
   (ecryptfs--wrapped-passphrase-file) (ecryptfs--passphrase)))

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
  (if (not (and (file-exists-p (ecryptfs--wrapped-passphrase-file))
                (file-exists-p (ecryptfs--mount-passphrase-sig-file))))
      (user-error "Encrypted private directory \"%s\" is not setup properly."
                  ecryptfs-private-dir-name)
    (let ((try-again t))
      (message "Encrypted filenames mode [%s]" (if (ecryptfs--encrypt-filenames-p) "ON" "OFF"))
      (while (and ;; In the first iteration, we try to silently mount the ecryptfs private directory,
              ;; this would succeed if the key is available in the keyring.
              (prog1 (not (zerop (shell-command ecryptfs--mount-private-cmd ecryptfs-buffer-name)))
                (message "Successfully mounted private directory."))
              (prog1 try-again (setq try-again nil)))
        (if (zerop (shell-command (ecryptfs--unwrap-passphrase-command) ecryptfs-buffer-name))
            (message "Successfully mounted private directory.")
          (user-error "A problem occured while mounting the private directory, see %s"
                      ecryptfs-buffer-name))))))

;;;###autoload
(defun ecryptfs-umount-private ()
  "Unmount eCryptfs' private directory."
  (interactive)
  (if (zerop (shell-command ecryptfs--umount-private-cmd ecryptfs-buffer-name))
      (message "Unmounted private directory successfully.")
    (user-error "Cannot unmount the private directory, seems to be already unmounted.")))

;;; ecryptfs.el ends here
