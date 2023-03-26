;;; me-builtin.el --- Customization of some of Emacs' builtin libraries -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")


(use-package transient
  :straight (:type built-in)
  :config
  ;; Map ESC and q to quit transient
  (define-key transient-map [escape]  #'transient-quit-one)
  (define-key transient-map (kbd "q") #'transient-quit-one))


(provide 'me-builtin)
