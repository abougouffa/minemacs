;;; me-debug.el --- Debugging stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(use-package gdb-mi
  :straight (:type built-in)
  :defer t
  :custom
  (gdb-show-main t) ; display source file containing main routine at startup
  (gdb-many-windows t) ; start in gdb-many-windows mode
  (gdb-debug-log-max 1024) ; default 128
  (gdb-restore-window-configuration-after-quit t)
  (gdb-thread-buffer-verbose-names nil)
  (gdb-window-configuration-directory (+directory-ensure (concat minemacs-local-dir "gdb/")))
  (gdb-max-source-window-count 1) ; IDEA maybe increase it!
  (gdb-display-io-nopopup nil)) ; IDEA maybe change it!

(use-package realgud
  :straight t
  :defer t
  :general
  (+map-local :keymaps '(c-mode-map c++-mode-map python-mode-map
                         sh-mode-map bash-ts-mode-map)
    "d" '(nil :wk "realgud")
    "dd" #'+realgud:start
    "dh" #'+realgud-hydra)
  :config
  ;; Add some missing gdb/rr commands
  (defun +realgud:cmd-start (arg)
    "start = break main + run"
    (interactive "p")
    (realgud-command "start"))

  (defun +realgud:cmd-reverse-next (arg)
    "Reverse next"
    (interactive "p")
    (realgud-command "reverse-next"))

  (defun +realgud:cmd-reverse-step (arg)
    "Reverse step"
    (interactive "p")
    (realgud-command "reverse-step"))

  (defun +realgud:cmd-reverse-continue (arg)
    "Reverse continue"
    (interactive "p")
    (realgud-command "reverse-continue"))

  (defun +realgud:cmd-reverse-finish (arg)
    "Reverse finish"
    (interactive "p")
    (realgud-command "reverse-finish"))

  ;; Define a hydra binding
  (defhydra +realgud-hydra (:color pink :hint nil :foreign-keys run)
    "
 Stepping  |  _n_: next      |  _i_: step    |  _o_: finish  |  _c_: continue  |  _R_: restart  |  _u_: until-here
 Revese    | _rn_: next      | _ri_: step    | _ro_: finish  | _rc_: continue  |
 Breakpts  | _ba_: break     | _bD_: delete  | _bt_: tbreak  | _bd_: disable   | _be_: enable   | _tr_: backtrace
 Eval      | _ee_: at-point  | _er_: region  | _eE_: eval    |
           |  _!_: shell     | _Qk_: kill    | _Qq_: quit    | _Sg_: gdb       | _Ss_: start
"
    ("n"  realgud:cmd-next)
    ("i"  realgud:cmd-step)
    ("o"  realgud:cmd-finish)
    ("c"  realgud:cmd-continue)
    ("R"  realgud:cmd-restart)
    ("u"  realgud:cmd-until-here)
    ("rn" +realgud:cmd-reverse-next)
    ("ri" +realgud:cmd-reverse-step)
    ("ro" +realgud:cmd-reverse-finish)
    ("rc" +realgud:cmd-reverse-continue)
    ("ba" realgud:cmd-break)
    ("bt" realgud:cmd-tbreak)
    ("bD" realgud:cmd-delete)
    ("be" realgud:cmd-enable)
    ("bd" realgud:cmd-disable)
    ("ee" realgud:cmd-eval-at-point)
    ("er" realgud:cmd-eval-region)
    ("tr" realgud:cmd-backtrace)
    ("eE" realgud:cmd-eval)
    ("!"  realgud:cmd-shell)
    ("Qk" realgud:cmd-kill)
    ("Sg" realgud:gdb)
    ("Ss" +realgud:cmd-start)
    ("q"  nil "quit" :color blue) ;; :exit
    ("Qq" realgud:cmd-quit :color blue))) ;; :exit

(use-package realgud-lldb
  :straight t
  :general
  (+map-local :keymaps 'rust-mode-map
    "d" #'+realgud:start)
  :commands realgud--lldb)

(use-package realgud-ipdb
  :straight t
  :defer t)

(use-package disaster
  :straight t
  :general
  (+map-local :keymaps '(c-mode-map c++-mode-map fortran-mode-map)
    "D" #'disaster))


(provide 'me-debug)
