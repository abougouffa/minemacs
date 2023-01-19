;; -*- lexical-binding: t; -*-


(use-package emojify
  :straight t
  :hook (minemacs-lazy . global-emojify-mode)
  :general
  (+map "ie" '(emojify-insert-emoji :wk "Emoji"))
  :custom
  (emojify-emoji-set "emojione-v2.2.6")
  (emojify-emojis-dir (concat minemacs-cache-dir "emojify/emojis/"))
  (emojify-display-style 'image)
  (emojify-download-emojis-p t))
