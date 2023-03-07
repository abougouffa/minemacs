;; minemacs-lazy.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

;; Virtual module loaded when idle after `minemacs-loaded'.
;; Used to synchronize loading some other stuff after loading Emacs.

;; The hooks in `minemacs-lazy-hook' are loaded incrementally when Emacs goes
;; idle, but when `minemacs-not-lazy' is set to t, they will be all loaded at
;; once.

;; Run hooks
(when minemacs-lazy-hook
  ;; Reverse the order to follow the order in which modules are loaded. Make
  ;; sure `gcmh-mode' is the last to be called. The `gc-cons-threshold' has been
  ;; set in "early-init.el" to a ridiculously high value to reduce the number of
  ;; garbage collections at startup, it will be overwritten by `gcmh-mode', so
  ;; we defer loading it to the end to maximize the benefit.
  (setq minemacs-lazy-hook (append (delq 'gcmh-mode (reverse minemacs-lazy-hook)) '(gcmh-mode)))
  (if minemacs-not-lazy
      (progn ; If `minemacs-no-lazy' is bound and true, force loading lazy hooks immediately
        (+log! "Loading %d lazy packages immediately."
               (length minemacs-lazy-hook))
        (run-hooks 'minemacs-lazy-hook))
    (+log! "Loading %d lazy packages incrementally." (length minemacs-lazy-hook))
    ;; Run hooks one by one, as a FIFO.
    (apply #'+eval-when-idle (append '(1) minemacs-lazy-hook))))

(+log! "Providing `minemacs-lazy'.")

(provide 'minemacs-lazy)
