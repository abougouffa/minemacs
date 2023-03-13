;;; me-emojify.el --- Emojify integration for Emacs28 and earlier -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")


(use-package emojify
  :straight t
  :hook (minemacs-lazy . global-emojify-mode)
  :init
  (+map! "ie" '(emojify-insert-emoji :wk "Emoji"))
  :custom
  (emojify-emoji-set "emojione-v2.2.6")
  (emojify-emojis-dir (concat minemacs-cache-dir "emojify/emojis/"))
  (emojify-display-style 'image)
  (emojify-download-emojis-p t))
