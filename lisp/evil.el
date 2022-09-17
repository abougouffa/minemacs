;; -*- lexical-binding: t; -*-

(setq evil-want-integration t
      evil-want-keybinding nil
      evil-want-C-i-jump nil
      evil-respect-visual-line-mode t)

(use-package evil :straight t)
(use-package evil-collection :straight t)
(use-package evil-nerd-commenter :straight t)
(use-package evil-mc :straight t)

;; C-h is backspace in insert state
(setq evil-want-C-h-delete t
      evil-undo-system 'undo-redo ;; Assumes Emacs 28, otherwise use undo-tree
      evil-want-Y-yank-to-eol t
      evil-want-fine-undo t)

;; Enable Evil globally
(evil-mode 1)

;; Enable multi cursors
(global-evil-mc-mode 1)
