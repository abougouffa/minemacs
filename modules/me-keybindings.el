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
  ;; Allow a key binding to be modified by multiple rules in
  ;; `which-key-replacement-alist'
  (which-key-allow-multiple-replacements t)
  :config
  (cl-callf append which-key-replacement-alist
    (list
     '(("\\`g z" . "\\`evil-\\(?:mc\\|multiedit\\)-\\(.*\\)")    . (nil . "⌶·\\1"))
     '(("\\`g c" . "\\`evilnc-\\(.*\\)")                         . (nil . "#·\\1"))
     '(("\\`g" . "\\`[Ii]nfo[-:]?\\(?:a-\\)?\\(.*\\)")           . (nil . "ɩ·\\1"))
     '(("\\`SPC TAB" . "\\`tabspaces-\\(.*\\)")                  . (nil . "⭾·\\1"))
     '(("\\`SPC p" . "\\`\\+?\\(?:consult-\\)?project-\\(.*\\)") . (nil . "π·\\1"))
     '(("" . "\\`evil[-:]?\\(?:a-\\)?\\(.*\\)")                  . (nil . "ɛ·\\1"))))
  ;; Setup `which-key' integration with the minibuffer
  (which-key-setup-minibuffer))

(use-package hydra
  :straight t)

(use-package avy
  :straight t
  :bind (("C-\"" . avy-goto-char)
         ("C-é" . avy-goto-line) ; French AZERTY
         ("M-g l" . avy-goto-line)))


(provide 'me-keybindings)

;;; me-keybindings.el ends here
