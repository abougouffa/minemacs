;; -*- lexical-binding: t; -*-

(use-package evil
  :straight t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-i-jump nil
        evil-respect-visual-line-mode t
        evil-want-C-h-delete t ;; C-h is backspace in insert state
        evil-want-Y-yank-to-eol t
        evil-want-fine-undo t)
  :config
  (evil-mode 1))


(use-package evil-collection
  :after evil
  :straight t
  :config
  (evil-collection-init))


(use-package evil-nerd-commenter
  :after evil minemacs-loaded
  :straight t)


(use-package evil-mc
  :straight t
  :after evil evil-collection
  :config
  ;; (evil-collection-swap-key nil 'evil-normal-state-map
  ;;   "gz" "gZ")
  (me-map-def
    :states '(normal visual)
    :keymaps 'evil-mc-key-map
    :prefix "gz"
    "" '(nil :which-key "evil-mc")
    "d" #'evil-mc-make-and-goto-next-match
    "D" #'evil-mc-make-and-goto-prev-match
    "s" #'evil-mc-skip-and-goto-next-match
    "S" #'evil-mc-skip-and-goto-prev-match
    "c" #'evil-mc-skip-and-goto-next-cursor
    "C" #'evil-mc-skip-and-goto-prev-cursor
    "j" #'evil-mc-make-cursor-move-next-line
    "k" #'evil-mc-make-cursor-move-prev-line
    "m" #'evil-mc-make-all-cursors
    "n" #'evil-mc-make-and-goto-next-cursor
    "N" #'evil-mc-make-and-goto-last-cursor
    "p" #'evil-mc-make-and-goto-prev-cursor
    "P" #'evil-mc-make-and-goto-first-cursor
    "q" #'evil-mc-undo-all-cursors)
  (me-map-def
    :states 'visual
    :keymaps 'evil-mc-key-map
    :prefix "gz"
    "i" #'evil-mc-make-cursor-in-visual-selection-beg
    "a" #'evil-mc-make-cursor-in-visual-selection-end)
  ;; Add support to repeat these commands when prefixed with a number
  (dolist (cmd '(evil-mc-make-and-goto-first-cursor
                 evil-mc-make-and-goto-last-cursor
                 evil-mc-make-and-goto-prev-cursor
                 evil-mc-make-and-goto-next-cursor
                 evil-mc-skip-and-goto-prev-cursor
                 evil-mc-skip-and-goto-next-cursor
                 evil-mc-make-and-goto-prev-match
                 evil-mc-make-and-goto-next-match
                 evil-mc-skip-and-goto-prev-match
                 evil-mc-skip-and-goto-next-match))
    (advice-add
     cmd :around
     (lambda (fn)
       (dotimes (i (if (integerp current-prefix-arg) current-prefix-arg 1))
         (funcall fn)))))
  ;; Enable globally
  (global-evil-mc-mode 1))

(use-package evil-escape
  :after (evil minemacs-loaded)
  :straight t
  :config
  (evil-escape-mode 1))


(provide 'me-evil)
