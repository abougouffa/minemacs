;; minemacs-loaded.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

;; Virtual module loaded at end of init.el (after custom-vars.el)
;; Used to synchronize loading some other stuff after loading Emacs

;; Run hooks
(when minemacs-after-startup-hook
  (setq minemacs-after-startup-hook (reverse minemacs-after-startup-hook))
  (+log! "Running %d `minemacs-after-startup-hook' hooks."
         (length minemacs-after-startup-hook))
  (run-hooks 'minemacs-after-startup-hook))

(if minemacs-not-lazy
    (require 'minemacs-lazy)
  (+eval-when-idle-for! 2
    (require 'minemacs-lazy)))

(+log! "Providing `minemacs-loaded'.")

(provide 'minemacs-loaded)
