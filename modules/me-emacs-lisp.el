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

(defconst +parinfer-rust-path (+directory-ensure minemacs-local-dir "parinfer-rust/"))

(use-package parinfer-rust-mode
  :straight t
  :when (and (+emacs-features-p 'modules) ; Emacs built with `--with-modules' option
             (or (eq sys/arch 'x86_64) ; x86_64 modules can be downloaded as binaries
                 ;; it is always possible to compile the module yourself on other architectures
                 (directory-files +parinfer-rust-path nil (format "\\.%s$" (if os/win "dll" "so")))))
  :custom
  (parinfer-rust-library-directory +parinfer-rust-path)
  (parinfer-rust-auto-download (eq sys/arch 'x86_64))
  :hook ((lisp-data-mode clojure-mode scheme-mode racket-mode hy-mode janet-mode) . parinfer-rust-mode)
  :config
  (setq parinfer-rust-troublesome-modes (delq 'electric-pair-mode parinfer-rust-troublesome-modes))
  (defvar-local +parinter-rust--was-enabled-p nil)

  ;; HACK: Disable `parinfer-rust-mode' on some commands.
  (defun +parinter-rust--restore:after-a (&rest _)
    (when +parinter-rust--was-enabled-p
      (setq +parinter-rust--was-enabled-p nil)
      (parinfer-rust-mode 1)))

  (defun +parinter-rust--disable:before-a (&rest _)
    (if (and (bound-and-true-p parinfer-rust-mode) (bound-and-true-p parinfer-rust-enabled))
        (progn (setq +parinter-rust--was-enabled-p t)
               (parinfer-rust-mode -1))
      (setq +parinter-rust--was-enabled-p nil)))

  ;; The `evil-shif-right' (and `evil-shift-left' which uses it under the hood)
  ;; behave strangely when `parinfer-rust-mode' is enabled, so lets disable when
  ;; using this command.
  (dolist (cmd '(evil-shift-right))
    (advice-add cmd :before #'+parinter-rust--disable:before-a)
    (advice-add cmd :after #'+parinter-rust--restore:after-a)))

(use-package macrostep
  :straight t
  :init
  (+map-local! :keymaps '(emacs-lisp-mode-map lisp-mode-map)
    "m" '(macrostep-expand :wk "Expand macro")))

(use-package helpful
  :straight t
  :init
  (+map! :keymaps 'emacs-lisp-mode-map
    :infix "h"
    "p" #'helpful-at-point
    "o" #'helpful-symbol
    "c" #'helpful-command
    "F" #'helpful-function
    "f" #'helpful-callable)
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
      (eros--eval-overlay (+octave-eval-last-sexp) (point)))

    (+map-local! :keymaps 'octave-mode-map
      "e"  '(nil :wk "eval")
      "ee" #'+eros-octave-eval-last-sexp)))


(provide 'me-emacs-lisp)

;;; me-emacs-lisp.el ends here
