;;; me-builtin.el --- Customization of some of Emacs' builtin libraries -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package transient
  :straight (:type built-in)
  :config
  ;; Map ESC and q to quit transient
  (define-key transient-map [escape]  #'transient-quit-one)
  (define-key transient-map (kbd "q") #'transient-quit-one))

(use-package map
  :straight (:type built-in))

(use-package let-alist
  :straight (:type built-in))

(use-package password-cache
  :straight (:type built-in)
  :custom
  (password-cache t) ; Enable password caching
  (password-cache-expiry 60)) ; One minute, default is 16

(use-package auth-source
  :straight (:type built-in)
  :custom
  (auth-sources '("~/.authinfo.gpg")) ; Default auth-sources to GPG
  (auth-source-do-cache t) ; Enable caching, do not keep asking about GPG key
  (auth-source-cache-expiry 86400)) ; All day, default is 2h (7200)


(provide 'me-builtin)

;;; me-builtin.el ends here
