;; minemacs-loaded.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

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

(load "minemacs-lazy")

(+log! "Providing `minemacs-loaded'.")


(provide 'minemacs-loaded)

;;; minemacs-loaded.el ends here
