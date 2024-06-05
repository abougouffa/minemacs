;;; me-emacs-lisp.el --- Lisp, Scheme, Elisp -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package elisp-plus
  :straight (:host github :repo "abougouffa/elisp-plus")
  :after minemacs-first-elisp-file
  :init
  (elisp-plus-mode 1))

(use-package parinfer-rust-mode
  :straight t
  :when (+emacs-features-p 'modules)
  :hook ((lisp-mode emacs-lisp-mode clojure-mode scheme-mode racket-mode hy-mode janet-mode) . +parinfer-rust-mode-maybe)
  :custom
  (parinfer-rust-auto-download (eq sys/arch 'x86_64))
  :config
  (setq parinfer-rust-troublesome-modes (delq 'electric-pair-mode parinfer-rust-troublesome-modes))

  (defun +parinfer-rust-mode-maybe ()
    (when (or parinfer-rust-auto-download (file-exists-p (expand-file-name parinfer-rust--lib-name parinfer-rust-library-directory)))
      (parinfer-rust-mode 1))))

(use-package macrostep
  :straight t
  :bind (:map emacs-lisp-mode-map ("C-c m" . macrostep-expand)))

(use-package helpful
  :straight t
  :bind (("<remap> <describe-variable>" . helpful-variable)
         ("<remap> <describe-symbol>" . helpful-symbol)
         ("<remap> <describe-function>" . helpful-function)
         ("<remap> <describe-command>" . helpful-command)
         ("<remap> <describe-key>" . helpful-key)))

(use-package info-colors
  :straight t
  :hook (Info-selection . info-colors-fontify-node))

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
      (eros--eval-overlay (+octave-eval-last-sexp) (point)))))


(provide 'me-emacs-lisp)

;;; me-emacs-lisp.el ends here
