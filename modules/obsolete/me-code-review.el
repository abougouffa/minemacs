;;; me-code-review.el --- Review pull requests from Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package emojify ; Needed by `code-review'
  :straight t
  :custom
  (emojify-emoji-set "emojione-v2.2.6")
  (emojify-display-style 'image)
  (emojify-download-emojis-p t))

(use-package code-review
  :straight (:host github :repo "doomelpa/code-review")
  :after magit
  :custom
  (code-review-download-dir (concat minemacs-cache-dir "code-review/"))
  (code-review-auth-login-marker 'forge) ; use the same credentials as forge in ~/.authinfo.gpg
  :init
  (transient-append-suffix 'magit-merge "i" '("y" "Review pull-request" code-review-forge-pr-at-point))
  (with-eval-after-load 'forge
    (transient-append-suffix 'forge-dispatch "c u" '("c r" "review pull-request" code-review-forge-pr-at-point)))
  :config
  (require 'on-demand/me-markdown))


(provide 'obsolete/me-code-review)
;;; me-code-review.el ends here
