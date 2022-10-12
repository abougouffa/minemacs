;; me-evil.el --- MinEmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(use-package evil
  :straight t
  :custom
  (evil-want-C-i-jump nil)
  (evil-want-C-h-delete t) ;; C-h is backspace in insert state
  (evil-want-fine-undo t)
  (evil-want-keybinding nil)
  (evil-want-integration t)
  (evil-want-Y-yank-to-eol t)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-kill-on-visual-paste nil)
  (evil-respect-visual-line-mode t)
  :config
  ;; Better but may cause problems with org-fold
  ;; https://github.com/emacs-evil/evil/issues/1630
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-mode 1)
  ;; Ask for a buffer when splitting windows
  (with-eval-after-load 'consult
    (dolist (fn '(evil-window-split evil-window-vsplit))
      (advice-add fn :after (lambda (&rest _) (consult-buffer))))))


(use-package evil-collection
  :after evil
  :straight t
  :config
  (defvar +evil-collection-modes
    (me-filter
     (lambda (a) ;; Maybe add elisp-mode!
       (not (memq a '(evil-mc))))
     evil-collection-mode-list))
  (evil-collection-init +evil-collection-modes))


(use-package evil-nerd-commenter
  :straight t
  :after evil minemacs-loaded
  :general
  (me-map-key "gc" #'evilnc-comment-operator))


(use-package evil-mc
  :straight t
  :after evil-collection minemacs-loaded
  :init
  ;; We will redefine the keybindngs
  (defvar evil-mc-key-map (make-sparse-keymap))
  :config
  (me-map-key
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

  (me-map-key
    :states 'visual
    :keymaps 'evil-mc-key-map
    :prefix "gz"
    "i" #'evil-mc-make-cursor-in-visual-selection-beg
    "a" #'evil-mc-make-cursor-in-visual-selection-end)

  ;; Enable globally
  (global-evil-mc-mode 1)

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

  ;; Custom commands to execute with evil-mc
  (dolist (fn '((backward-kill-word)
                (corfu-complete . evil-mc-execute-default-complete)
                (undo-fu-only-undo . evil-mc-execute-default-undo)
                (undo-fu-only-redo . evil-mc-execute-default-redo)
                (evil-delete-back-to-indentation . evil-mc-execute-default-call)
                (evil-escape . evil-mc-execute-default-evil-normal-state) ;; C-g
                (evil-numbers/inc-at-pt-incremental)
                (evil-numbers/dec-at-pt-incremental)
                (evil-digit-argument-or-evil-beginning-of-visual-line
                 (:default . evil-mc-execute-default-call)
                 (visual . evil-mc-execute-visual-call))
                (ess-smart-comma . evil-mc-execute-call)
                (evil-org-delete . evil-mc-execute-default-evil-delete)))
    (setf (alist-get (car fn) evil-mc-custom-known-commands)
          (if (and (cdr fn) (listp (cdr fn)))
              (cdr fn)
            (list (cons :default
                        (or (cdr fn)
                            #'evil-mc-execute-default-call-with-count)))))))


(provide 'me-evil)
