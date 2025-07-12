;;; me-emacs-lisp.el --- Lisp, Scheme, Elisp -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2023-07-31
;; Last modified: 2025-07-12

;;; Commentary:

;;; Code:

;; Better Emacs Lisp code viewing
(use-package elisp-plus
  :straight (:host github :repo "abougouffa/elisp-plus")
  :after minemacs-first-elisp-file
  :custom
  (elisp-plus-better-lisp-indent nil)
  :init
  (elisp-plus-mode 1))


;; Simplifying how you write Lisp
(use-package parinfer-rust-mode
  :straight t
  :when (featurep 'feat/modules)
  :autoload +parinfer-rust-mode-maybe
  :custom
  (parinfer-rust-auto-download (featurep 'arch/x86_64))
  :hook ((lisp-mode emacs-lisp-mode clojure-mode scheme-mode racket-mode hy-mode janet-mode) . +parinfer-rust-mode-maybe)
  :config
  (defun +parinfer-rust-mode-maybe ()
    (when (or parinfer-rust-auto-download (file-exists-p parinfer-rust-library))
      ;; BUG+HACK: Defer applying `parinfer-rust-mode', this should fix the
      ;; issue of unusable `parinfer-rust-mode' until disabled and enabled again
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

  ;; HACK: Fix the issue of `vundo' (related to `track-changes') when exploring
  ;; the undo tree
  (with-eval-after-load 'vundo
    (add-hook 'vundo-pre-enter-hook #'+parinfer-rust--disable)
    (add-hook 'vundo-post-exit-hook #'+parinfer-rust--restore)))


;; A better Emacs *help* buffer
(use-package helpful
  :straight t
  :bind (("C-h h" . helpful-at-point) ; orig. `view-hello-file'
         ([remap describe-variable] . helpful-variable)
         ([remap describe-symbol] . helpful-symbol)
         ([remap describe-function] . helpful-callable)
         ([remap describe-command] . helpful-command)
         ([remap describe-key] . helpful-key))
  :config
  ;; HACK: Showing the source code of the symbol in the help buffer isn't always
  ;; helpful, especially for big definitions.
  (advice-add 'helpful--source :override #'ignore))


;; Evaluation Result OverlayS for Emacs Lisp
(use-package eros
  :straight t
  :hook (minemacs-first-elisp-file . eros-mode)
  :custom
  (eros-eval-result-prefix (if (char-displayable-p ?→) "→ " "=>")))


;; Elisp regexp mistake finder
(use-package relint
  :straight t)


(provide 'me-emacs-lisp)

;;; me-emacs-lisp.el ends here
