;; -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")


;; From: emacswiki.org/emacs/download/misc-cmds.el
(defun +read-shell-file-command (command)
  "Prompt for shell COMMAND, using current buffer's file as default arg.
If buffer is not associated with a file, you are prompted for a file.
COMMAND is a symbol."
  (let ((file (or (buffer-file-name) (read-file-name "File: "))))
    (setq file (and file (file-name-nondirectory file))
          command (format "%s  " command)) ; Convert to string.
    (read-from-minibuffer
     "" (cons (concat command (and file  (concat " " file))) (length command)))))

;;;###autoload
(defun +chmod-this-file (cmd)
  "Execute Unix command `chmod'.  Current buffer's file is default arg.
CMD is the command to execute (interactively, `chmod')."
  (interactive (list (+read-shell-file-command 'chmod)))
  (shell-command cmd))

;;;###autoload
(defun +chgrp-this-file (cmd)
  "Execute Unix command `chgrp'.  Current buffer's file is default arg.
CMD is the command to execute (interactively, `chgrp')."
  (interactive (list (+read-shell-file-command 'chgrp)))
  (shell-command cmd))

;;;###autoload
(defun +chown-this-file (cmd)
  "Execute Unix command `chown'.  Current buffer's file is default arg.
CMD is the command to execute (interactively, `chown')."
  (interactive (list (+read-shell-file-command 'chown)))
  (shell-command cmd))
