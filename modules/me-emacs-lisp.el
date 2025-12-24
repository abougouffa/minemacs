;;; me-emacs-lisp.el --- Lisp, Scheme, Elisp -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2023-07-31
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;; Better Emacs Lisp code viewing
(use-package elisp-plus
  :vc (:url "https://github.com/abougouffa/elisp-plus")
  :after minemacs-first-elisp-file
  :custom
  (elisp-plus-better-lisp-indent nil)
  :init
  (elisp-plus-mode 1))


;; Simplifying how you write Lisp
(use-package parinfer-rust-mode
  :ensure t
  :when (featurep 'feat/modules)
  :autoload +parinfer-rust-mode-maybe
  :custom
  (parinfer-rust-auto-download (featurep 'arch/x86_64))
  :hook ((lisp-mode emacs-lisp-mode clojure-mode scheme-mode racket-mode hy-mode janet-mode) . +parinfer-rust-mode-maybe)
  :config
  (cl-callf2 remq 'electric-pair-mode parinfer-rust-troublesome-modes) ; `electric-pair-mode' looks fine!
  (defun +parinfer-rust-mode-maybe ()
    (when (and (or parinfer-rust-auto-download (file-exists-p parinfer-rust-library))
               (not (+unresolved-merge-conflict-p)))
      ;; BUG+HACK: Defer calling `parinfer-rust-mode', this should fix the issue
      ;; of unusable `parinfer-rust-mode' until disabled and enabled again
      (run-with-timer
       0.5 nil
       (lambda (buffer)
         (while-no-input
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (unless buffer-read-only ; don't enable in read-only buffers
                 (if (get-buffer-window)
                     (parinfer-rust-mode 1)
                   (cl-letf (((symbol-function 'y-or-n-p) #'ignore)) ; Don't ask about modifying indentation in invisible buffers
                     (parinfer-rust-mode 1))))))))
       (current-buffer))))

  ;; HACK: Disable `parinfer-rust-mode' temporary on some commands.
  (defvar-local +parinfer-rust--was-enabled-p nil)

  (defun +parinfer-rust--restore (&rest _)
    (when +parinfer-rust--was-enabled-p
      (setq +parinfer-rust--was-enabled-p nil)
      (parinfer-rust-mode 1)
      (+info! "Restored `parinfer-rust-mode'")))

  (defun +parinfer-rust--disable (&rest _)
    (setq +parinfer-rust--was-enabled-p (bound-and-true-p parinfer-rust-mode))
    (when +parinfer-rust--was-enabled-p
      (parinfer-rust-mode -1)
      (+info! "Disabled `parinfer-rust-mode'")))

  ;; BUGFIX: Fix the issue of `vundo' (related to `track-changes') when exploring the undo tree
  (with-eval-after-load 'vundo
    (add-hook 'vundo-pre-enter-hook #'+parinfer-rust--disable)
    (add-hook 'vundo-post-exit-hook #'+parinfer-rust--restore)))


;; Evaluation Result OverlayS for Emacs Lisp
(use-package eros
  :ensure t
  :hook (minemacs-first-elisp-file . eros-mode)
  :custom
  (eros-eval-result-prefix (if (char-displayable-p ?→) "→ " "=>")))


(provide 'me-emacs-lisp)

;;; me-emacs-lisp.el ends here
