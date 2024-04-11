;;; me-code-review.el --- Review pull requests from Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package emojify ;; Needed by `code-review'
  :straight t
  :custom
  (emojify-emoji-set "emojione-v2.2.6")
  (emojify-emojis-dir (concat minemacs-cache-dir "emojify/emojis/"))
  (emojify-display-style 'image)
  (emojify-download-emojis-p t)
  :init
  (when (< emacs-major-version 29)
    (+map! "ie" '(emojify-insert-emoji :wk "Emoji"))))

(use-package code-review
  :straight (:host github :repo "phelrine/code-review" :branch "fix/closql-update")
  :after magit
  :custom
  (code-review-download-dir (concat minemacs-cache-dir "code-review/"))
  (code-review-db-database-file (concat minemacs-local-dir "code-review/database.sqlite"))
  (code-review-log-file (concat minemacs-local-dir "code-review/code-review-error.log"))
  (code-review-auth-login-marker 'forge) ; use the same credentials as forge in ~/.authinfo.gpg
  :init
  (transient-append-suffix 'magit-merge "i" '("y" "Review pull-request" code-review-forge-pr-at-point))
  (with-eval-after-load 'forge
    (transient-append-suffix 'forge-dispatch "c u" '("c r" "review pull-request" code-review-forge-pr-at-point))))


(provide 'obsolete/me-code-review)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; me-code-review.el ends here
