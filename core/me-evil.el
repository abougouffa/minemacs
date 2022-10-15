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
  :after evil minemacs-loaded
  :straight t
  :config
  (defvar +evil-collection-modes
    (me-filter
     (lambda (a)
       ;; elisp-mode uses gz to open ielm, which I never use!
       ;; and uses gr to xref-find-references
       (not (memq a '(elisp-mode evil-mc))))
     evil-collection-mode-list))
  (evil-collection-init +evil-collection-modes))


(use-package evil-nerd-commenter
  :straight t
  :after evil minemacs-loaded
  :general
  (me-map-key "gc" #'evilnc-comment-operator))


(use-package evil-mc
  :straight t
  :after evil-collection
  :init
  ;; We will redefine the keybindngs
  (defvar evil-mc-key-map (make-sparse-keymap))
  (defvar evil-mc-cursors-map (make-sparse-keymap))
  (defconst evil-collection-evil-mc-maps '(evil-mc-key-map evil-mc-cursors-map))

  (define-key evil-mc-cursors-map (kbd "u") 'evil-mc-undo-last-added-cursor)
  (define-key evil-mc-cursors-map (kbd "q") 'evil-mc-undo-all-cursors)
  (define-key evil-mc-cursors-map (kbd "P") 'evil-mc-pause-cursors)
  (define-key evil-mc-cursors-map (kbd "R") 'evil-mc-resume-cursors)
  (define-key evil-mc-cursors-map (kbd "m") 'evil-mc-make-all-cursors)
  (define-key evil-mc-cursors-map (kbd "f") 'evil-mc-make-and-goto-first-cursor)
  (define-key evil-mc-cursors-map (kbd "l") 'evil-mc-make-and-goto-last-cursor)
  (define-key evil-mc-cursors-map (kbd "n") 'evil-mc-make-and-goto-next-cursor)
  (define-key evil-mc-cursors-map (kbd "p") 'evil-mc-make-and-goto-prev-cursor)
  (define-key evil-mc-cursors-map (kbd "h") 'evil-mc-make-cursor-here)
  (define-key evil-mc-cursors-map (kbd "j") 'evil-mc-make-cursor-move-next-line)
  (define-key evil-mc-cursors-map (kbd "k") 'evil-mc-make-cursor-move-prev-line)
  (define-key evil-mc-cursors-map (kbd "s") 'evil-mc-skip-and-goto-next-cursor)
  (define-key evil-mc-cursors-map (kbd "S") 'evil-mc-skip-and-goto-prev-cursor)
  (define-key evil-mc-cursors-map (kbd "c") 'evil-mc-skip-and-goto-next-match)
  (define-key evil-mc-cursors-map (kbd "C") 'evil-mc-skip-and-goto-prev-match)
  (define-key evil-mc-cursors-map (kbd "i") 'evil-mc-make-cursor-in-visual-selection-beg)
  (define-key evil-mc-cursors-map (kbd "a") 'evil-mc-make-cursor-in-visual-selection-end)

  (evil-define-key '(normal visual) 'evil-mc-key-map (kbd "gz") evil-mc-cursors-map)

  :commands (evil-mc-undo-last-added-cursor
             evil-mc-undo-all-cursors
             evil-mc-pause-cursors
             evil-mc-resume-cursors
             evil-mc-make-all-cursors
             evil-mc-make-and-goto-first-cursor
             evil-mc-make-and-goto-last-cursor
             evil-mc-make-and-goto-next-cursor
             evil-mc-make-and-goto-prev-cursor
             evil-mc-make-cursor-here
             evil-mc-make-cursor-move-next-line
             evil-mc-make-cursor-move-prev-line
             evil-mc-skip-and-goto-next-cursor
             evil-mc-skip-and-goto-prev-cursor
             evil-mc-skip-and-goto-next-match
             evil-mc-skip-and-goto-prev-match
             evil-mc-make-cursor-in-visual-selection-beg
             evil-mc-make-cursor-in-visual-selection-end)

  :config
  ;; HACK (from Doom Emacs),
  ;; evil-mc's design is bizarre. Its variables and hooks are lazy loaded
  ;; rather than declared at top-level, some hooks aren't defined or
  ;; documented, it's a bit initializer-function drunk, and its minor modes
  ;; are intended to be perpetually active -- even when no cursors are active.
  ;; I undo all of that here.
  (evil-mc-define-vars)
  (evil-mc-initialize-vars)
  (add-hook 'evil-mc-before-cursors-created #'evil-mc-pause-incompatible-modes)
  (add-hook 'evil-mc-before-cursors-created #'evil-mc-initialize-active-state)
  (add-hook 'evil-mc-after-cursors-deleted  #'evil-mc-teardown-active-state)
  (add-hook 'evil-mc-after-cursors-deleted  #'evil-mc-resume-incompatible-modes)
  (advice-add #'evil-mc-initialize-hooks :override #'ignore)
  (advice-add #'evil-mc-teardown-hooks :override #'evil-mc-initialize-vars)
  (advice-add #'evil-mc-initialize-active-state :before #'turn-on-evil-mc-mode)
  (advice-add #'evil-mc-teardown-active-state :after #'turn-off-evil-mc-mode)
  (setq cursor-type t)
  (setq evil-default-cursor 'box)
  ;; https://github.com/gabesoft/evil-mc/issues/70
  (add-hook
   'evil-mc-after-cursors-deleted
   (lambda ()
     (setq evil-was-yanked-without-register t)))

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
