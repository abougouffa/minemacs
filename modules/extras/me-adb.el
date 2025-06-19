;;; me-adb.el --- Interface for ADB commands -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-05-22
;; Last modified: 2025-06-19

;;; Commentary:

;;; Code:

(defvar +adb-buffer-name "*adb*")
(defvar +adb-process-name "adb-command")
(defvar +adb-push-src-dest-cache nil)
(defvar +adb-push-dest-history nil)
(defvar +adb-after-command-functions nil
  "Functions to call after the command finishes.")

(defvar +adb--buffer nil)
(defun +adb--after-command (buff desc)
  (when (and buff (eq buff +adb--buffer))
    (run-hook-with-args '+adb-after-command-functions (equal desc "finished\n") buff)))

(with-eval-after-load 'compile
  (add-hook 'compilation-finish-functions '+adb--after-command))

;;;###autoload
(progn
  (defvar +adb-program "adb")
  (defvar +adb-available-p nil)
  (defun +adb-available-p (&rest _args)
    (with-memoization +adb-available-p
      (and (executable-find +adb-program) t))))

;;;###autoload
(dolist (cmd '(+adb-push +adb-remount +adb-reboot +adb-root))
  (put cmd 'completion-predicate #'+adb-available-p))

(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables '+adb-push-src-dest-cache)
  (add-to-list 'savehist-additional-variables '+adb-push-dest-history))

(defun +adb-run-command (&rest args)
  "Run adb with command ARGS."
  (let ((display-buffer-overriding-action
         '((display-buffer-in-side-window) (window-height . 0.2) (reusable-frames . visible) (dedicated . t) (side . bottom) (slot . -1)))
        (compilation-buffer-name-function (lambda (&rest _args) +adb-buffer-name))
        (cmd (string-join `(,+adb-program ,@(seq-filter #'identity args)) " ")))
    (setq +adb--buffer (compile cmd))))

;;;###autoload
(defun +adb-push (src dest)
  "Run adb push SRC DEST."
  (interactive
   (let* ((src-path (if current-prefix-arg (read-file-name "Source path: ") (buffer-file-name)))
          (dest-path (alist-get (expand-file-name src-path) +adb-push-src-dest-cache nil nil #'equal)))
     (list src-path
           (read-string "Destination path: " dest-path '+adb-push-dest-history))))
  (let ((src (expand-file-name src)))
    (setq +adb-push-src-dest-cache (+alist-set src dest +adb-push-src-dest-cache))
    (+adb-run-command "push" src dest)))

;;;###autoload
(defun +adb-remount (auto-reboot-device)
  "Run adb remount, with -R when AUTO-REBOOT-DEVICE is non-nil."
  (interactive "P")
  (+adb-run-command "remount" (when auto-reboot-device "-R")))

;;;###autoload
(defun +adb-reboot (&optional mode no-confirm)
  "Run adb reboot MODE, when NO-CONFIRM is non-nil, don't ask."
  (interactive (list (and current-prefix-arg (completing-read "Reboot in mode: " '("bootloader" "recovery" "sideload" "sideload-auto-reboot" "edl")))))
  (when (or no-confirm (y-or-n-p "Do you really want to reboot the device? "))
    (+adb-run-command "reboot" mode)))

;;;###autoload
(defun +adb-root (&optional arg)
  "Run adb root (or unroot with \\[universal-argument])."
  (interactive "P")
  (+adb-run-command (if arg "unroot" "root")))


(provide 'me-adb)
;;; me-adb.el ends here
