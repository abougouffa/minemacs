;;; me-debug.el --- Debugging stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

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
    "b" #'dape-toggle-breakpoint
    "r" #'dape-remove-breakpoint-at-point
    "R" #'dape-remove-all-breakpoints
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
      ("bb" "Toggle" dape-toggle-breakpoint :transient t)
      ("bd" "Remove at pt" dape-remove-breakpoint-at-point :transient t)
      ("bD" "Remove all" dape-remove-all-breakpoints :transient t)
      ("bl" "Log" dape-log-breakpoint :transient t)]
     ["Info"
      ("ii" "Info" dape-info :transient t)
      ("im" "Memory" dape-read-memory :transient t)
      ("is" "Select Stack" dape-select-stack :transient t)
      ("R"  "Repl" dape-repl :transient t)]
     ["Quit"
      ("qq" "Quit" dape-quit :transient nil)
      ("qk" "Kill" dape-kill :transient nil)]]))

(use-package realgud
  :straight (realgud :build (:not compile))
  :init
  (+map-local! :keymaps '(c-mode-map c++-mode-map python-mode-map
                          c-ts-mode-map c++-ts-mode-map python-ts-mode-map
                          rust-mode-map rust-ts-mode-map
                          sh-mode-map bash-ts-mode-map)
    "r" '(nil :wk "realgud")
    "rd" #'+realgud:start
    "rh" #'+realgud-hydra/body))

(use-package realgud-lldb
  :straight t
  :init
  (defalias 'realgud:lldb #'realgud--lldb)
  :commands (realgud--lldb realgud:lldb lldb))

(use-package realgud-ipdb
  :straight t
  :commands (ipdb realgud:ipdb))

(use-package disaster
  :straight t
  :when (executable-find "objdump")
  :init
  (+map-local! :keymaps '(c-mode-map c++-mode-map fortran-mode-map)
    "D" #'disaster))


(provide 'me-debug)

;;; me-debug.el ends here
