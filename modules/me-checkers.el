;;; me-checkers.el --- Syntax checking -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package flymake
  :straight (:type built-in)
  :init
  (+map! "tf" #'flymake-mode)
  :custom
  (flymake-fringe-indicator-position 'right-fringe)
  (flymake-error-bitmap '(+flymake-bitmap-left-arrow-hi-res compilation-error))
  (flymake-warning-bitmap '(+flymake-bitmap-left-arrow-hi-res compilation-warning))
  (flymake-note-bitmap '(+flymake-bitmap-left-arrow-hi-res compilation-info))
  :config
  (+map-local! :keymaps 'flymake-mode-map
    "f"  '(nil :wk "flymake")
    "ff" #'+flymake-main/body
    "fn" #'flymake-goto-next-error
    "fN" #'flymake-goto-prev-error
    "fs" #'flymake-start
    "fb" #'flymake-show-buffer-diagnostics
    "fp" #'flymake-show-project-diagnostics)

  ;; Use the session's load-path with flymake
  (setq elisp-flymake-byte-compile-load-path
        (append elisp-flymake-byte-compile-load-path load-path))

  ;; Larger right frings
  (set-fringe-style '(8 . 13))

  ;; Better fringe bitmaps
  (define-fringe-bitmap '+flymake-bitmap-arrow
    [#b11111000
     #b01111100
     #b00111110
     #b00011111
     #b00111110
     #b01111100
     #b11111000])
  (define-fringe-bitmap '+flymake-bitmap-arrow-hi-res
    [#b01111000000
     #b00111100000
     #b00011110000
     #b00001111000
     #b00000111100
     #b00000011110
     #b00000011110
     #b00000111100
     #b00001111000
     #b00011110000
     #b00111100000
     #b01111000000]
    nil 13)
  (define-fringe-bitmap '+flymake-bitmap-left-arrow-hi-res
    [#b00000011110
     #b00000111100
     #b00001111000
     #b00011110000
     #b00111100000
     #b01111000000
     #b01111000000
     #b00111100000
     #b00011110000
     #b00001111000
     #b00000111100
     #b00000011110]
    nil 13)

  (defhydra +flymake-main (:color red :hint nil :foreign-keys warn)
    "
[Flymake]                                              [_q_] quit
  ├──────────────────────────────────────────────────────────────────────╮
  │  [_B_] Buffer diagnostics  [_P_] Project diagnostics  [_L_] Log buffer     │
  │  [_n_] Next error          [_N_] Prev error           [_S_] Start          │
  ╰──────────────────────────────────────────────────────────────────────╯
"
    ("B" flymake-show-buffer-diagnostics)
    ("P" flymake-show-project-diagnostics)
    ("L" flymake-switch-to-log-buffer)
    ("n" flymake-goto-next-error)
    ("N" flymake-goto-prev-error)
    ("S" flymake-start)
    ("q" nil :color blue)))

(use-package flymake-easy
  :straight t)


(provide 'me-checkers)

;;; me-checkers.el ends here
