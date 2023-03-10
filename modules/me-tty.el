;;; me-tty.el --- Emacs from terminal -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(use-package xt-mouse
  :straight (:type built-in)
  :hook (tty-setup . xterm-mouse-mode))

(use-package xclip
  :straight t
  :when os/linux
  :defines +xclip--enable-in-tty-h
  :hook (tty-setup . +xclip--enable-in-tty-h)
  :config
  (defun +xclip--enable-in-tty-h ()
    (with-demoted-errors "%s" (xclip-mode 1))))


(provide 'me-tty)
