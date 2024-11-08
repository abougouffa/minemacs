;;; me-checkers.el --- Syntax checking -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;; Collection of checkers for Flymake
(use-package flymake-collection
  :straight (:fork (:repo "abougouffa/flymake-collection" :branch "additional-checkers"))
  :init
  (satch-add-hook 'prog-mode-hook #'flymake-collection-hook-setup nil nil :transient t)
  :custom
  (flymake-collection-hook-inherit-config t))


;; Transient menu for Flymake
(use-package flymenu
  :straight (:host github :repo "KarimAziev/flymenu"))


;; Flymake backend for PMD, the extensible cross-language static code analyzer
(use-package flymake-pmd
  :straight (:host github :repo "rody/flymake-pmd")
  :custom
  (flymake-pmd-pmd-6-app-name "pmd")
  :config
  ;; Use the PMD 6 format when we have the right version installed
  (setq flymake-pmd-use-pmd-6
        (and (executable-find flymake-pmd-executable-name)
             (when-let* ((ver-out (shell-command-to-string (format "%s --version" flymake-pmd-executable-name)))
                         (ver (string-match "PMD \\(?1:\\([0-9]*\\.\\)*[0-9]*\\)" ver-out)))
               (version<= (match-string 1 ver-out) "6.0")))))


;; A `flymake' integration with the linting library (`package-lint') for Elisp package metadata
(use-package package-lint-flymake
  :straight t
  :hook (emacs-lisp-mode . package-lint-flymake-setup))


(provide 'me-checkers)

;;; me-checkers.el ends here
