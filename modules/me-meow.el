;; me-meow.el --- Meow: a modal editing mode for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:


(use-package meow
  :straight t
  :hook (minemacs-lazy . +meow-activate)
  :config
  (defun +meow-setup/azerty ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("(" . meow-beginning-of-thing)
     '(")" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("F" . +meow-backward-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("P" . meow-yank-pop)
     '("q" . +meow-quit-dwim)
     '("Q" . meow-quit)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("S" . meow-kill-whole-line)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     ;; v/V are used by `expreg' below
     '("_" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . ignore) ;; TODO
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("$" . meow-line)
     '("'" . repeat)
     '("<escape>" . ignore)))

  (defun +meow-backward-find ()
    (interactive)
    (let ((current-prefix-arg -1))
      (call-interactively 'meow-find)))

  (defun +meow-quit-dwim ()
    "Like `meow-quit', but doesn't quit file-visiting buffers/windows."
    (interactive)
    (unless (buffer-file-name) (meow-quit)))

  (defun +meow-activate ()
    (when (bound-and-true-p evil-mode)
      (warn "Meow isn't supposed to be used with Evil"))
    (+meow-setup/azerty)
    (meow-global-mode 1))

  ;; Quit the `corfu' auto-completion on exiting the insert state
  (with-eval-after-load 'corfu
    (add-hook 'meow-insert-exit-hook #'corfu-quit))

  ;; Start `git-commit-mode' in insert state
  (with-eval-after-load 'git-commit
    (+add-hook! git-commit-mode (when (bound-and-true-p meow-mode) (meow-insert-mode 1))))

  ;; Bound to "k", but "k" is used as "up" in motion state
  (with-eval-after-load 'magit
    (keymap-set magit-mode-map "*" #'magit-delete-thing))

  ;; Bind `expreg-expand' to "v" in normal state
  (unless (+package-disabled-p 'expreg 'me-editor)
    (meow-normal-define-key
     '("v" . expreg-expand)
     '("V" . expreg-contract))))


(provide 'me-meow)
;;; me-meow.el ends here
