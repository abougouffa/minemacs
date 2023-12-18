;;; me-tty.el --- Emacs from terminal -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package xclip
  :elpaca t
  :hook (tty-setup . +xclip--enable-in-tty-h)
  :config
  (defun +xclip--enable-in-tty-h ()
    (with-demoted-errors "%s" (xclip-mode 1))))


(provide 'me-tty)

;;; me-tty.el ends here
