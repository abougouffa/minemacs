;; minemacs-loaded.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-10-02
;; Last modified: 2026-04-02

;;; Commentary:

;;; Code:

;; Run hooks
(when minemacs-after-startup-hook
  (setq minemacs-after-startup-hook (reverse minemacs-after-startup-hook))
  (+log! "Running %d `minemacs-after-startup-hook' hooks."
         (length minemacs-after-startup-hook))
  (with-demoted-errors "[MinEmacs:AfterStartupLoadError] %s"
    (run-hooks 'minemacs-after-startup-hook)))

(+log! "Providing `minemacs-loaded'.")

(provide 'minemacs-loaded)

(when minemacs-lazy-hook
  (if minemacs-not-lazy-p
      (progn ; If `minemacs-not-lazy-p' is true, force loading lazy hooks immediately
        (+log! "Loading %d lazy packages immediately." (length minemacs-lazy-hook))
        (with-demoted-errors "[MinEmacs:LazyLoadError] %s"
          (run-hooks 'minemacs-lazy-hook))
        (provide 'minemacs-lazy))
    (+log! "Loading %d lazy packages incrementally." (length minemacs-lazy-hook))
    (cl-callf2 append (mapcar #'ensure-list minemacs-lazy-hook)
               minemacs--deferred-forms
      '((provide 'minemacs-lazy))))) ;; Provide `minemacs-lazy' at the end

(defvar minemacs--lazy-high-priority-timer
  (run-with-timer
   0.1 0.001
   (lambda ()
     (if minemacs--deferred-forms
         (let ((inhibit-message (not minemacs-verbose-p))
               (form (pop minemacs--deferred-forms)))
           (with-demoted-errors "[MinEmacs:DeferredLoadError] %s"
             (eval form)))
       (cancel-timer minemacs--lazy-high-priority-timer)))))


;;; minemacs-loaded.el ends here
