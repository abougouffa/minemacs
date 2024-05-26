;; me-gc.el --- MinEmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;; The `gc-cons-threshold' has been set in "early-init.el" to a ridiculously
;; high value (`most-positive-fixnum') to reduce the number of garbage
;; collections during startup, it will be overwritten by `gcmh-mode' or by the
;; following hook, so we place it at the end of `minemacs-lazy-hook' to maximize
;; the benefit.

;; NOTE: I'm experimenting with these settings instead of using `gcmh'.
;; See: https://zenodo.org/records/10213384
(when (+package-disabled-p 'gcmh)
  (add-hook
   'minemacs-lazy-hook
   (satch-defun +minemacs--gc-tweaks-h ()
     (setq gc-cons-threshold (* 128 1024 1024)
           gc-cons-percentage 0.25))
   90))

(use-package gcmh
  :straight t
  :init
  (add-hook 'minemacs-lazy-hook #'gcmh-mode 90)
  :custom
  ;; Set the delay to 20s instead of the default 15. I tried using `auto', but
  ;; with the default 20 of `gcmh-auto-idle-delay-factor', it triggers GC each
  ;; 1s on my machine. Setting the factor to a higher value should solve the
  ;; issue on my machine, but I don't think it is right to assume it will work
  ;; the same way on other machines. So we switch back to a fixed delay of 20s.
  (gcmh-idle-delay 20)
  ;; The default `gcmh's 1GB is probably too high. We set it to 256MB on 64bit
  ;; systems, or 16MB on 32bit ones.
  (gcmh-high-cons-threshold (* 1024 1024 (if (string-suffix-p "64" (symbol-name sys/arch)) 256 16))))


(provide 'me-gc)

;;; me-gc.el ends here
