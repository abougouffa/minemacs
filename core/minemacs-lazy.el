;; minemacs-lazy.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;; Feature loaded when Emacs is idle after `minemacs-loaded', it is used to
;; lazily load other stuff after loading Emacs.

;; The hooks in `minemacs-lazy-hook' are loaded incrementally when Emacs goes
;; idle, but when `minemacs-not-lazy-p' is set to t, they will be all loaded at
;; once.

;;; Code:

;; Run hooks
(when minemacs-lazy-hook
  ;; Reverse the order to follow the order in which modules are loaded. Make
  ;; sure `gcmh-mode' is the last to be called. The `gc-cons-threshold' has been
  ;; set in "early-init.el" to a ridiculously high value to reduce the number of
  ;; garbage collections during startup, it will be overwritten by `gcmh-mode',
  ;; so we defer loading it to the end to maximize the benefit.
  (setq minemacs-lazy-hook (append (delq 'gcmh-mode (reverse minemacs-lazy-hook)) '(gcmh-mode)))
  (if minemacs-not-lazy-p
      (progn ; If `minemacs-not-lazy-p' is true, force loading lazy hooks immediately
        (+log! "Loading %d lazy packages immediately." (length minemacs-lazy-hook))
        (run-hooks 'minemacs-lazy-hook))
    (+log! "Loading %d lazy packages incrementally." (length minemacs-lazy-hook))
    ;; Run hooks one by one, as a FIFO.
    (apply #'+eval-when-idle (append '(1) minemacs-lazy-hook))))

(+log! "Providing `minemacs-lazy'.")


(provide 'minemacs-lazy)

;;; minemacs-lazy.el ends here
