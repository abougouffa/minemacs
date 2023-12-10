;; minemacs-loaded.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa and contributors

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;; This feature is loaded at end of init.el (after loading custom-vars.el), it
;; is used to synchronize loading some other stuff after loading Emacs

;;; Code:

;; Run hooks
(when minemacs-after-startup-hook
  (setq minemacs-after-startup-hook (reverse minemacs-after-startup-hook))
  (+log! "Running %d `minemacs-after-startup-hook' hooks."
         (length minemacs-after-startup-hook))
  (run-hooks 'minemacs-after-startup-hook))

(if minemacs-not-lazy-p
    (require 'minemacs-lazy)
  (+eval-when-idle-for! 2
    (require 'minemacs-lazy)))

(+log! "Providing `minemacs-loaded'.")


(provide 'minemacs-loaded)

;;; minemacs-loaded.el ends here
