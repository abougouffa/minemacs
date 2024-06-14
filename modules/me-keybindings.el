;;; me-keybindings.el --- Default keybindings -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package which-key
  :straight t
  :hook (minemacs-lazy . which-key-mode)
  :custom
  (which-key-idle-delay 1.0)
  (which-key-idle-secondary-delay nil)
  (which-key-ellipsis "..")
  (which-key-prefix-prefix "+")
  (which-key-sort-order #'which-key-key-order-alpha)
  (which-key-min-display-lines 3)
  (which-key-max-display-columns nil)
  (which-key-allow-multiple-replacements t) ; Allow multiple rules in `which-key-replacement-alist'
  :config
  ;; Setup `which-key' integration with the minibuffer
  (which-key-setup-minibuffer))

(use-package hydra
  :straight t)

(use-package key-chord
  :straight t)


(provide 'me-keybindings)

;;; me-keybindings.el ends here
