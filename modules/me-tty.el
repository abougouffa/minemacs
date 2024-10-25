;;; me-tty.el --- Emacs from terminal -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;; Copy&paste GUI clipboard from text terminal
(use-package xclip
  :straight t
  :hook (tty-setup . +xclip--enable-in-tty-h)
  :config
  (defun +xclip--enable-in-tty-h ()
    (let ((inhibit-message t))
      (with-demoted-errors "%s" (xclip-mode 1)))))


(provide 'me-tty)

;;; me-tty.el ends here
