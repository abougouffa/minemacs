;;; me-files.el --- File management -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;; Collection of useful dired additions
(use-package dired-hacks
  :straight t)


;; Sort and browse disk usage listings
(use-package disk-usage
  :straight t)


;; View, edit, search and compare very large files in batches, trading memory for processor time
(use-package vlf-setup
  :straight (vlf :source gnu-elpa-mirror)
  :demand
  :config
  (with-eval-after-load 'so-long
    (add-to-list 'so-long-mode-preserved-variables 'vlf-mode)))


;; Fast opening of large files
(use-package guard-lf
  :straight (:host github :repo "jcs-elpa/guard-lf")
  :init
  (guard-lf-mode 1)
  :config
  (cl-callf append guard-lf-intact-major-modes '(rosbag-info-mode ein:ipynb-mode)))


;; Utilities for opening files with "sudo"
(use-package sudo-edit
  :straight t
  :hook (minemacs-first-file . sudo-edit-indicator-mode)
  :commands (+sudo-edit-save)
  :config
  (defun +sudo-edit-save (choose-user save-as)
    "Save currently visited file/buffer as another user.

When called with \\[universal-argument], prompt for the user, otherwise,
save as `sudo-edit-user'. When called with \\[universal-argument] \\[universal-argument],
write to a new file name."
    (interactive (let ((num (prefix-numeric-value current-prefix-arg)))
                   (list (> num 1) (> num 4))))
    (if-let* ((user (if choose-user
                        (completing-read "User: " (and (fboundp 'system-users) (system-users)) nil nil nil 'sudo-edit-user-history sudo-edit-user)
                      sudo-edit-user))
              (file (or (and (or (not buffer-file-name) save-as)
                             (read-file-name (format "Save (as %S) to: " sudo-edit-user)))
                        buffer-file-name))
              (file (sudo-edit-filename file user))
              (dest-buffer (find-file-noselect file))
              (src-buffer (current-buffer)))
        (progn
          (copy-to-buffer dest-buffer (point-min) (point-max))
          (unwind-protect (with-current-buffer dest-buffer (save-buffer))
            (unless (eq src-buffer dest-buffer) (kill-buffer dest-buffer))
            (with-current-buffer src-buffer (revert-buffer t t))))
      (user-error "Unable to open %S" (abbreviate-file-name file)))))


;; Asynchronous "rsync" from `dired'
(use-package dired-rsync
  :straight (:files ("dired-rsync.el" "dired-rsync-transient.el"))
  :bind (:map
         dired-mode-map
         ("C-c C-r" . dired-rsync)
         ("C-c C-x" . dired-rsync-transient)))


;; Same functionality as `find-dired' and `find-grep-dired', using fd/rg instead
(use-package fd-dired
  :straight t)


;; Directory tree comparison mode for Emacs (inspired by commercial tools like Beyond Compare and Araxis Merge)
(use-package ztree
  :straight t
  :bind (:map minemacs-open-thing-map ("z" . ztree-diff))
  :init
  (with-eval-after-load 'ztree-view
    (setq ztree-draw-unicode-lines t)
    (keymap-set ztree-mode-map "n" #'ztree-next-line)
    (keymap-set ztree-mode-map "p" #'ztree-previous-line)))


(provide 'me-files)

;;; me-files.el ends here
