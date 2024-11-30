;;; me-emacs-lisp.el --- Lisp, Scheme, Elisp -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

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
  :when (+emacs-options-p 'modules)
  :autoload +parinfer-rust-mode-maybe
  :custom
  (parinfer-rust-auto-download (+emacs-options-p 'arch/x86_64))
  :hook ((lisp-mode emacs-lisp-mode clojure-mode scheme-mode racket-mode hy-mode janet-mode) . +parinfer-rust-mode-maybe)
  :config
  (defun +parinfer-rust-mode-maybe ()
    (when (or parinfer-rust-auto-download (file-exists-p (expand-file-name parinfer-rust--lib-name parinfer-rust-library-directory)))
      ;; BUG+HACK: Defer applying `parinfer-rust-mode', this should fix the
      ;; issue of unusable `parinfer-rust-mode' until disabled and enabled again
      (run-with-timer
       0.5 nil
       (lambda (buffer)
         (when (and (buffer-live-p buffer) (get-buffer-window buffer)) ; The buffer still alive and it is visible
           (with-current-buffer buffer
             ;; When calling `parinfer-rust-mode' in a non-visible buffer, don't
             ;; ask about modifying indentation, assume we said no.
             (if (get-buffer-window)
                 (parinfer-rust-mode 1)
               (cl-letf (((symbol-function 'y-or-n-p) #'ignore))
                 (parinfer-rust-mode 1))))))
       (current-buffer))))

  ;; HACK: Disable `parinfer-rust-mode' on some commands.
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


;; Interactive macro-expander for Emacs
(use-package macrostep
  :straight t
  :bind (:map emacs-lisp-mode-map ("C-c m" . macrostep-expand)))


;; A better Emacs *help* buffer
(use-package helpful
  :straight t
  :bind (("<remap> <describe-variable>" . helpful-variable)
         ("<remap> <describe-symbol>" . helpful-symbol)
         ("<remap> <describe-function>" . helpful-callable)
         ("<remap> <describe-command>" . helpful-command)
         ("<remap> <describe-key>" . helpful-key)
         ("C-h h" . helpful-at-point))) ; orig. `view-hello-file'


;; Inspection tool for Emacs Lisp objects
(use-package inspector
  :straight t)


;; Evaluation Result OverlayS for Emacs Lisp
(use-package eros
  :straight t
  :hook (minemacs-first-elisp-file . eros-mode)
  :custom
  (eros-eval-result-prefix (if (char-displayable-p ?→) "→ " "=>"))
  :config
  ;; Add an Elisp-like evaluation for Octave
  (with-eval-after-load 'octave
    (defun +eros-octave-eval-last-sexp ()
      "Wrapper for `+octave-eval-last-sexp' that overlays results."
      (interactive)
      (eros--make-result-overlay (+octave-eval-last-sexp) :where (point) :duration eros-eval-result-duration))))


;; Elisp regexp mistake finder
(use-package relint
  :straight t)


(provide 'me-emacs-lisp)

;;; me-emacs-lisp.el ends here
