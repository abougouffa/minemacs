;;; me-tty.el --- Emacs from terminal -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


;; TTY specific stuff
(add-hook 'tty-setup-hook #'xterm-mouse-mode)

(use-package xclip
  :straight t
  :when os/linux
  :defines +xclip--enable-in-tty-h
  :hook (tty-setup . +xclip--enable-in-tty-h)
  :config
  (defun +xclip--enable-in-tty-h ()
    (with-demoted-errors "%s" (xclip-mode 1))))


(provide 'me-tty)
