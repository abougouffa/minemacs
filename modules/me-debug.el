;;; me-debug.el --- Debugging stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package dape
  :straight (:host github :repo "svaante/dape")
  :commands +dape-transient
  :custom
  (dape-inline-variables t)
  (dape-adapter-dir (concat minemacs-local-dir "dape/"))
  :init
  (+map! :infix "d"
    "d" #'dape
    "n" #'dape-next
    "s" #'dape-step-in
    "o" #'dape-step-out
    "c" #'dape-continue
    "r" #'dape-restart
    "p" #'dape-pause
    "b" #'dape-breakpoint-toggle
    "e" #'dape-breakpoint-expression
    "r" #'dape-remove-breakpoint-at-point
    "R" #'dape-breakpoint-remove-all
    "t" #'+dape-transient
    "q" #'dape-kill
    "Q" #'dape-quit)
  :config
  (transient-define-prefix +dape-transient ()
    "Transient for dape."
    [["Stepping"
      ("n"  "Next" dape-next :transient t)
      ("s"  "Step in" dape-step-in :transient t)
      ("o"  "Step out" dape-step-out :transient t)
      ("c"  "Continue" dape-continue :transient t)
      ("r"  "Restart" dape-restart :transient t)]
     ["Breakpoints"
      ("bb" "Toggle" dape-breakpoint-toggle :transient t)
      ("be" "Expression" dape-breakpoint-expression :transient t)
      ("bd" "Remove at pt" dape-remove-breakpoint-at-point :transient t)
      ("bD" "Remove all" dape-breakpoint-face :transient t)
      ("bl" "Log" dape-breakpoint-log :transient t)]
     ["Info"
      ("ii" "Info" dape-info :transient t)
      ("im" "Memory" dape-read-memory :transient t)
      ("is" "Select Stack" dape-select-stack :transient t)
      ("R"  "Repl" dape-repl :transient t)]
     ["Quit"
      ("qq" "Quit" dape-quit :transient nil)
      ("qk" "Kill" dape-kill :transient nil)]]))

(use-package disaster
  :straight t
  :init
  (+map-local! :keymaps '(c-mode-map c++-mode-map fortran-mode-map)
    "D" #'disaster))

(use-package rmsbolt
  :straight t
  :config
  (+mode-alist-add-ts-modes! rmsbolt-languages))

(use-package beardbolt
  :straight (:host github :repo "abougouffa/beardbolt" :files ("*.el" "starters"))
  :hook (beardbolt--asm-mode . flymake-mode-off)
  :config
  (+mode-alist-add-ts-modes! beardbolt-languages))

(use-package objdump-disassemble
  :straight (:host github :repo "abougouffa/objdump-disassemble"))


(provide 'me-debug)

;;; me-debug.el ends here
