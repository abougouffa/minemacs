;;; me-adb.el --- Interface for ADB commands -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-05-22
;; Last modified: 2025-05-22

;;; Commentary:

;;; Code:

(defvar +adb-buffer-name "*adb*")
(defvar +adb-no-quote nil)
(defvar +adb-push-dest-history nil)

(defun +adb-run-command (&rest args)
  (let ((out-buf (get-buffer-create +adb-buffer-name)))
    (async-shell-command (string-join `("adb" ,@(mapcar (if +adb-no-quote #'identity #'shell-quote-argument) args)) " ") out-buf)
    (pop-to-buffer out-buf)))

;;;###autoload
(defun +adb-push (src dest)
  "Run adb push SRC DEST."
  (interactive (list (if current-prefix-arg
                         (read-file-name "Source path: ")
                       (buffer-file-name))
                     (read-string "Destination path: " nil '+adb-push-dest-history)))
  (+adb-run-command "push" (expand-file-name src) dest))

;;;###autoload
(defun +adb-remount ()
  "Run adb remount."
  (interactive)
  (+adb-run-command "remount"))


(provide 'me-adb)
;;; me-adb.el ends here
