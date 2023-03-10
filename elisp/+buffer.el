;; -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


;; From: https://www.emacswiki.org/emacs/download/misc-cmds.el
;; Candidate as a replacement for `kill-buffer', at least when used interactively.
;; For example: (define-key global-map [remap kill-buffer] 'kill-buffer-and-its-windows)
;; We cannot just redefine `kill-buffer', because some programs count on a
;; specific other buffer taking the place of the killed buffer (in the window).
;;;###autoload
(defun +kill-buffer-and-its-windows (buffer &optional msgp)
  "Kill BUFFER and delete its windows.  Default is `current-buffer'.
BUFFER may be either a buffer or its name (a string)."
  (interactive (list (read-buffer "Kill buffer: " (current-buffer) 'existing) 'MSGP))
  (setq buffer (get-buffer buffer))
  (if (buffer-live-p buffer) ; Kill live buffer only.
      (let ((wins (get-buffer-window-list buffer nil t))) ; On all frames.
        (when (kill-buffer buffer) ; Only delete windows if buffer killed.
          (dolist (win wins) ; (User might keep buffer if modified.)
            (when (window-live-p win)
              ;; Ignore error, in particular,
              ;; "Attempt to delete the sole visible or iconified frame".
              (condition-case nil (delete-window win) (error nil))))))
    (when msgp (user-error "Cannot kill buffer.  Not a live buffer: `%s'" buffer))))

;;;###autoload
(defun +region-to-buffer (start end buffer arg)
  "Copy region to BUFFER: At beginning (prefix >= 0), end (< 0), or replace.
START and END are the region boundaries.
BUFFER is a buffer or its name (a string).
With prefix ARG >= 0: `append-to-buffer':
  Append contents of region to end of BUFFER.
  (Point is moved to end of BUFFER first.)
With prefix ARG < 0:  `prepend-to-buffer':
  Prepend contents of region to beginning of BUFFER.
  (Point is moved to beginning of BUFFER first.)
With no prefix ARG (nil): `copy-to-buffer'.
  Write region to BUFFER, replacing any previous contents."
  (interactive
   (let ((arg (and current-prefix-arg (prefix-numeric-value current-prefix-arg))))
     (list (region-beginning)
           (region-end)
           (read-buffer
            (concat (if arg
                        (if (natnump arg) "Append" "Prepend")
                      "Write")
                    " region to buffer: ")
            (if (fboundp 'another-buffer) ; Defined in `misc-fns.el'.
                (another-buffer nil t)
              (other-buffer (current-buffer))))
           arg)))
  (setq buffer (get-buffer-create buffer)) ; Convert to buffer.
  (when (eq buffer (current-buffer)) (error "Cannot copy region to its own buffer"))
  (cond ((natnump arg)
         (with-current-buffer buffer (goto-char (point-max)))
         (append-to-buffer buffer start end))
        (arg
         (with-current-buffer buffer (goto-char (point-min)))
         (prepend-to-buffer buffer start end))
        (t (copy-to-buffer buffer start end))))

;;;###autoload
(defun +region-to-file (start end filename arg)
  "With prefix arg, this is `append-to-file'.  Without, it is `write-region'.
START and END are the region boundaries.
Prefix ARG non-nil means append region to end of file FILENAME.
Prefix ARG nil means write region to FILENAME, replacing contents."
  (interactive
   (list (region-beginning)
         (region-end)
         (read-file-name (concat (if current-prefix-arg "Append" "Write")
                                 " region to file: "))
         current-prefix-arg))
  (let* ((curr-file (buffer-file-name))
         (same-file-p (and curr-file (string= curr-file filename))))
    (cond ((or (not same-file-p)
               (progn (when (fboundp 'flash-ding) (flash-ding))
                      (yes-or-no-p
                       (format
                        "Do you really want to REPLACE the contents of `%s' by just the REGION? "
                        (file-name-nondirectory curr-file)))))
           (write-region start end filename arg)
           (when same-file-p (revert-buffer t t)))
          (t (message "OK.  Not written.")))))

;;;###autoload
(defun +kill-some-buffers (&optional list)
  "Kill some buffers.  Asks the user whether to kill the modified ones.
Non-interactively, if optional argument LIST is non-nil, it
specifies the list of buffers to kill, asking for approval for each one.
See `kill-some-buffers'."
  (interactive)
  ;; Replace the `kill-buffer-ask' locally (used by `kill-some-buffers')
  ;; with our function which don't ask about unmodified buffers.
  (cl-letf (((symbol-function 'kill-buffer-ask) #'+kill-buffer-ask-if-modified))
    (kill-some-buffers list)))

(defvar +kill-buffer-no-ask-list
  (append
   (list messages-buffer-name "*Warnings*")
   (when (featurep 'native-compile)
     (list comp-async-buffer-name comp-log-buffer-name)))
  "A list of buffer names to be killed without confirmation.")

;;;###autoload
(defun +kill-buffer-ask-if-modified (buffer)
  "Like `kill-buffer-ask', but kills BUFFER without confirmation if buffer is unmodified.
Kill without asking for buffer names in `+kill-buffer-no-ask-list'."
  (when (or (not (buffer-modified-p buffer))
            (member (buffer-name buffer) +kill-buffer-no-ask-list)
            (yes-or-no-p (format "Buffer %s HAS BEEN MODIFIED.  Kill? "
                                 (buffer-name buffer))))
    (kill-buffer buffer)))
