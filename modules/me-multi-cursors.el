;;; me-multi-cursors.el --- Programming stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package iedit
  :straight t)

(use-package evil-multiedit ; This will load `iedit' and suppresses it
  :straight t
  :unless (+package-disabled-p 'evil 'me-evil)
  :after evil minemacs-first-file
  :demand t
  :init
  (+nvmap! :infix "g"
    "ze" '(nil :wk "evil-multiedit")
    "zee" #'evil-multiedit-match-all
    "zer" #'evil-multiedit-restore
    "zeq" #'evil-multiedit-abort
    "zen" #'evil-multiedit-next
    "zeN" #'evil-multiedit-prev
    "zet" #'evil-multiedit-toggle-or-restrict-region)
  (+vmap! :infix "g"
    "zed" #'evil-multiedit-match-and-next
    "zeD" #'evil-multiedit-match-and-prev)
  (+nmap! :infix "g"
    "zed" #'evil-multiedit-match-symbol-and-next
    "zeD" #'evil-multiedit-match-symbol-and-prev
    "zeT" #'evil-multiedit-toggle-marker-here)
  :config
  (evil-multiedit-default-keybinds))

(use-package evil-iedit-state
  :straight t
  :unless (+package-disabled-p 'evil 'me-evil)
  :commands evil-iedit-state/iedit-mode
  :after iedit
  :init
  ;; Use the `iedit' key to trigger `evil-iedit-state/iedit-mode'.
  (when iedit-toggle-key-default
    (keymap-global-set (key-description iedit-toggle-key-default) 'evil-iedit-state/iedit-mode))
  :config
  ;; FIX: When we press "C-;" (`iedit-toggle-key-default') to enter `iedit-mode'
  ;; and then "C-;" to quit it, `evil-iedit-state' will stay in `iedit-mode'
  ;; even if the selections aren't displayed and no `iedit' indication is
  ;; displayed in minibuffer.
  (when iedit-toggle-key-default
    (keymap-set evil-iedit-state-map (key-description iedit-toggle-key-default) 'evil-iedit-state/quit-iedit-mode)))

(use-package evil-mc
  :straight t
  :unless (+package-disabled-p 'evil 'me-evil)
  :hook (minemacs-first-file . global-evil-mc-mode)
  :config
  ;; Use "gz" instead of "gr", this last is mapped to `xref-find-references' in some programming modes.
  (evil-define-key* '(normal visual) evil-mc-key-map (kbd "gr") nil)
  (evil-define-key* '(normal visual) evil-mc-key-map (kbd "gz") evil-mc-cursors-map)

  ;; Add support to repeat these commands when prefixed with a number
  (dolist (cmd '(evil-mc-make-and-goto-first-cursor evil-mc-make-and-goto-last-cursor
                 evil-mc-make-and-goto-prev-cursor evil-mc-make-and-goto-next-cursor
                 evil-mc-skip-and-goto-prev-cursor evil-mc-skip-and-goto-next-cursor
                 evil-mc-make-and-goto-prev-match evil-mc-make-and-goto-next-match
                 evil-mc-skip-and-goto-prev-match evil-mc-skip-and-goto-next-match))
    (advice-add cmd :around (lambda (fn) (dotimes (i (if (integerp current-prefix-arg) current-prefix-arg 1)) (funcall fn)))))

  ;; Custom commands to execute with `evil-mc'
  (setq evil-mc-custom-known-commands
        '((backward-kill-word (:default . evil-mc-execute-default-call-with-count))
          (evil-delete-back-to-indentation (:default . evil-mc-execute-default-call))
          (undo-fu-only-redo (:default . evil-mc-execute-default-redo))
          (undo-fu-only-undo (:default . evil-mc-execute-default-undo))
          (corfu-complete (:default . evil-mc-execute-default-complete))
          (evil-numbers/dec-at-pt-incremental (:default . evil-mc-execute-default-call-with-count))
          (evil-numbers/inc-at-pt-incremental (:default . evil-mc-execute-default-call-with-count))
          (evil-org-delete (:default . evil-mc-execute-default-evil-delete))
          (ess-smart-comma (:default . evil-mc-execute-call))
          (evil-digit-argument-or-evil-beginning-of-visual-line
           (:default . evil-mc-execute-default-call)
           (visual . evil-mc-execute-visual-call)))))


(provide 'me-multi-cursors)

;;; me-multi-cursors.el ends here
