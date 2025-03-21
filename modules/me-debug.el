;;; me-debug.el --- Debugging stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;; Debug Adapter Protocol for Emacs
(use-package dape
  :straight t
  :commands (+dape-transient)
  :hook
  ((kill-emacs . dape-breakpoint-save) ; Save breakpoints on quit
   (dape-compile . kill-buffer) ; Kill compile buffer on build success
   (dape-display-source . pulse-momentary-highlight-one-line) ; Pulse source line (performance hit)
   (dape-stopped . dape-info) ; To display info and/or repl buffers on stopped
   (dape-stopped . dape-repl)
   (dape-start . (lambda () (save-some-buffers t t)))) ; Save buffers on startup, useful for interpreted languages
  :config
  (dape-breakpoint-load) ; Load breakpoints on startup, with laziness
  (with-eval-after-load 'transient
    (transient-define-prefix +dape-transient ()
      "Transient for dape."
      [["Stepping"
        ("n"  "Next"         dape-next                       :transient t)
        ("s"  "Step in"      dape-step-in                    :transient t)
        ("o"  "Step out"     dape-step-out                   :transient t)
        ("c"  "Continue"     dape-continue                   :transient t)
        ("p"  "Pause"        dape-pause                      :transient t)
        ("r"  "Restart"      dape-restart                    :transient t)]
       ["Breakpoints"
        ("bb" "Toggle"       dape-breakpoint-toggle          :transient t)
        ("be" "Expression"   dape-breakpoint-expression      :transient t)
        ("bd" "Remove at pt" dape-breakpoint-remove-at-point :transient t)
        ("bD" "Remove all"   dape-breakpoint-remove-all      :transient t)
        ("bh" "Hits"         dape-breakpoint-hits            :transient t)
        ("bl" "Log"          dape-breakpoint-log             :transient t)]
       ["Info"
        ("ii" "Info"         dape-info                       :transient t)
        ("im" "Memory"       dape-read-memory                :transient t)
        ("is" "Select Stack" dape-select-stack               :transient t)
        ("iw" "Watch DWIM"   dape-evaluate-expression        :transient t)
        ("ie" "Eval expr."   dape-watch-dwim                 :transient t)
        ("R"  "Repl"         dape-repl                       :transient t)]
       ["Quit"
        ("qq" "Quit"         dape-quit                       :transient nil)
        ("qk" "Kill"         dape-kill                       :transient nil)
        ("qd" "Disconnect"   dape-disconnect-quit            :transient nil)]])))


;; A compiler output viewer
(use-package rmsbolt
  :straight t
  :config
  (+mode-alist-add-ts-modes! rmsbolt-languages))


;; Compiler Explorer clone (fork of `rmsbolt' optimized for C/C++)
(use-package beardbolt
  :straight (:host github :repo "joaotavora/beardbolt" :files (:defaults "starters"))
  :hook (beardbolt--asm-mode . flymake-mode-off)
  :config
  (+mode-alist-add-ts-modes! beardbolt-languages))


;; Use "objdump" to display disassembled executable and object files
(use-package objdump-disassemble
  :straight (:host github :repo "abougouffa/objdump-disassemble"))


(provide 'me-debug)

;;; me-debug.el ends here
