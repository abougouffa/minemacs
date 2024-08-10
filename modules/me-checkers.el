;;; me-checkers.el --- Syntax checking -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package flymake-collection
  :straight (:fork (:repo "abougouffa/flymake-collection" :branch "additional-checkers"))
  :init
  (satch-add-hook 'prog-mode-hook #'flymake-collection-hook-setup nil nil :transient t)
  :custom
  (flymake-collection-hook-inherit-config t))

(use-package flymake-cppcheck
  :straight (:host github :repo "shaohme/flymake-cppcheck")
  :init
  (when (executable-find "cppcheck")
    (satch-add-hook '(c-mode-hook c-ts-mode-hook c++-mode-hook c++-ts-mode-hook) #'flymake-cppcheck-setup)))

(use-package flymenu
  :straight (:host github :repo "KarimAziev/flymenu"))

(use-package flymake-guile
  :straight (:source emacsmirror-mirror)
  :init
  (when (executable-find "guild") (add-hook 'scheme-mode-hook #'flymake-guile)))

(use-package flymake-pmd
  :straight (:host github :repo "rody/flymake-pmd")
  :custom
  (flymake-pmd-use-eglot t) ; Integrate with Eglot results
  :config
  ;; Use the PMD 6 format when we have the right version installed
  (setq flymake-pmd-use-pmd-6
        (and (executable-find flymake-pmd-executable-name)
             (when-let* ((ver-out (shell-command-to-string (format "%s --version" flymake-pmd-executable-name)))
                         (ver (string-match "PMD \\(?1:\\([0-9]*\\.\\)*[0-9]*\\)" ver-out)))
               (version<= "6.0" (match-string 1 ver-out))))))


(provide 'me-checkers)

;;; me-checkers.el ends here
