;;; lisp.el --- Lisp, Scheme, Elisp -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

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
  :hook (emacs-lisp-mode clojure-mode scheme-mode lisp-mode racket-mode hy-mode)
  :config
  (setq parinfer-rust-troublesome-modes (delq 'electric-pair-mode parinfer-rust-troublesome-modes))
  (defvar-local +parinter-rust--was-enabled-p nil)

  ;; HACK: Disable `parinfer-rust-mode' on some commands.
  (defun +parinter-rust--restore-a (&rest _)
    (when +parinter-rust--was-enabled-p
      (setq +parinter-rust--was-enabled-p nil)
      (parinfer-rust-mode 1)))

  (defun +parinter-rust--disable-a (&rest _)
    (if (and (bound-and-true-p parinfer-rust-mode) (bound-and-true-p parinfer-rust-enabled))
        (progn (setq +parinter-rust--was-enabled-p t)
               (parinfer-rust-mode -1))
      (setq +parinter-rust--was-enabled-p nil)))

  ;; The `evil-shif-right' (and `evil-shift-left' which uses it under the hood)
  ;; behave strangely when `parinfer-rust-mode' is enabled, so lets disable when
  ;; using this command.
  (dolist (cmd '(evil-shift-right))
    (advice-add cmd :before #'+parinter-rust--disable-a)
    (advice-add cmd :after #'+parinter-rust--restore-a)))

(use-package macrostep
  :straight t
  :init
  (+map-local! :keymaps '(emacs-lisp-mode-map lisp-mode-map)
    "m" '(macrostep-expand :wk "Expand macro")))

(use-package elisp-demos
  :straight t
  :after elisp-mode minemacs-loaded
  :demand t
  :init
  (+map! :infix "he"
    "d" #'elisp-demos-find-demo
    "D" #'elisp-demos-add-demo)
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package helpful
  :straight t
  :init
  (+map! :keymaps 'emacs-lisp-mode-map
    :infix "h"
    "p" #'helpful-at-point
    "o" #'helpful-symbol
    "c" #'helpful-command
    "F" #'helpful-function
    "f" #'helpful-callable))

(use-package info-colors
  :straight t
  :hook (Info-selection . info-colors-fontify-node))

(use-package eros
  :straight t
  :after elisp-mode minemacs-loaded
  :demand t
  :custom
  (eros-eval-result-prefix "―→ ")
  :config
  (eros-mode 1)

  ;; Add an Elisp-like evaluation for Octave
  (with-eval-after-load 'octave
    (defun +eros-octave-eval-last-sexp ()
      "Wrapper for `+octave-eval-last-sexp' that overlays results."
      (interactive)
      (eros--eval-overlay (+octave-eval-last-sexp) (point)))

    (+map-local! :keymaps 'octave-mode-map
      "e"  '(nil :wk "eval")
      "ee" #'+eros-octave-eval-last-sexp)))


(provide 'me-lisp)

;;; me-lisp.el ends here
