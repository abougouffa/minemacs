;; minemacs-loaded.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;; Run hooks
(when minemacs-after-startup-hook
  (setq minemacs-after-startup-hook (reverse minemacs-after-startup-hook))
  (+log! "Running %d `minemacs-after-startup-hook' hooks."
         (length minemacs-after-startup-hook))
  (run-hooks 'minemacs-after-startup-hook))

(+log! "Providing `minemacs-loaded'.")

(provide 'minemacs-loaded)

(when minemacs-lazy-hook
  (if minemacs-not-lazy-p
      (progn ; If `minemacs-not-lazy-p' is true, force loading lazy hooks immediately
        (+log! "Loading %d lazy packages immediately." (length minemacs-lazy-hook))
        (run-hooks 'minemacs-lazy-hook)
        (provide 'minemacs-lazy))
    (+log! "Loading %d lazy packages incrementally." (length minemacs-lazy-hook))
    (cl-callf append minemacs--lazy-high-priority-forms
      (mapcar #'ensure-list minemacs-lazy-hook)
      '((provide 'minemacs-lazy))))) ;; Provide `minemacs-lazy' at the end

(defvar minemacs--lazy-high-priority-timer
  (run-with-timer
   0.1 0.001
   (lambda ()
     (if minemacs--lazy-high-priority-forms
         (let ((inhibit-message (not minemacs-verbose-p)))
           (eval (pop minemacs--lazy-high-priority-forms)))
       (cancel-timer minemacs--lazy-high-priority-timer)))))

(defvar minemacs--lazy-low-priority-timer
  (run-with-timer
   0.3 0.001
   (lambda ()
     (if minemacs--lazy-low-priority-forms
         (let ((inhibit-message (not minemacs-verbose-p)))
           (eval (pop minemacs--lazy-low-priority-forms)))
       (cancel-timer minemacs--lazy-low-priority-timer)))))

;;; minemacs-loaded.el ends here
