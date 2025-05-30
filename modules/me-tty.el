;;; me-tty.el --- Emacs from terminal -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-12-24
;; Last modified: 2025-03-21

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
