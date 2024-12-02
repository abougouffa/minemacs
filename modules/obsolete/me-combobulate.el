;;; me-combobulate.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:


;; Structured editing and navigation in Emacs with Tree-Sitter
(use-package combobulate-setup
  ;; TEMP: The "combobulate.el" contains a lot of autoloads that prevent lazy loading, we exclude the main file
  :vc (combobulate :url "https://github.com/mickeynp/combobulate" :ignored-files ("combobulate.el" "tests/*"))
  :when (and (not (+emacs-options-p 'os/win)) (+emacs-options-p 'tree-sitter)) ; TEMP: disable on Windows
  :custom
  (combobulate-key-prefix "C-c b") ; "C-c o" is used by `minemacs-open-thing-map'
  :config
  ;; TEMP+FIX: Basically, load the same features that would be loaded by "combobulate.el"
  (dolist (feature '(combobulate-rules
                     combobulate-procedure combobulate-navigation
                     combobulate-manipulation combobulate-envelope combobulate-display
                     combobulate-ui combobulate-misc combobulate-query combobulate-cursor
                     combobulate-toml combobulate-html combobulate-python combobulate-js-ts
                     combobulate-css combobulate-yaml combobulate-json combobulate-go))
    (require feature))

  ;; The "M-<up/down/left/right>" keys are used globally by `drag-stuff', lets
  ;; unset them for `combobulate' and use "M-S-<up/down/left/right>" instead.
  (mapc (lambda (k) (keymap-unset combobulate-key-map k 'remove)) '("M-<up>" "M-<down>" "M-<left>" "M-<right>"))
  (keymap-set combobulate-key-map "M-S-<up>" #'combobulate-splice-up)
  (keymap-set combobulate-key-map "M-S-<down>" #'combobulate-splice-down)
  (keymap-set combobulate-key-map "M-S-<left>" #'combobulate-splice-self)
  (keymap-set combobulate-key-map "M-S-<right>" #'combobulate-splice-parent))


(provide 'obsolete/me-combobulate)
;;; me-combobulate.el ends here
