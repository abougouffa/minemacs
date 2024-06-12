;;; me-realgud.el --- Extra commands for RealGUD with better evil integration -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;; Evil integraion has been adopted from Doom Emacs
;; github.com/doomemacs/doomemacs/blob/master/modules/tools/debugger/autoload/evil.el

;;; Code:


(use-package realgud
  :straight (:build (:not compile)))

(use-package realgud-lldb
  :straight t
  :init
  (defalias 'realgud:lldb #'realgud--lldb)
  :commands (realgud--lldb realgud:lldb lldb))

(use-package realgud-ipdb
  :straight t
  :commands (ipdb realgud:ipdb))


;;;###autoload
(defun +realgud:start (&optional path)
  "Start the RealGUD debugger suitable for the current mode."
  (interactive (list (when evil-called-from-ex-p (evil-ex-file-arg)))) ;; <=> `evil-define-command' with (interactive "<f>")
  (let ((default-directory
         (or (and (project-current) (project-root (project-current)))
             (and (fboundp 'projectile-project-root) (projectile-project-root))
             (vc-root-dir)
             default-directory)))
    (pcase major-mode
      ((or 'c-mode 'c++-mode 'c-ts-mode 'c++-ts-mode
           'objc-mode 'fortran-mode 'ada-mode 'modula-2-mode
           'd-mode 'opencl-mode 'go-mode 'go-ts-mode)
       (realgud:gdb (if path (concat "gdb " path))))
      ((or 'rust-mode 'rust-ts-mode)
       (lldb (if path (concat "lldb " path))))
      ((or 'js-mode 'js2-mode 'js3-mode 'typescript-mode 'js-ts-mode 'typescript-ts-mode)
       (realgud:trepanjs))
      ((or 'sh-mode 'bash-ts-mode)
       (pcase sh-shell
         ((or "bash" "sh")
          (realgud:bashdb (if path (concat "bashdb " path))))
         ("zsh"
          (realgud:zshdb (if path (concat "zshdb " path))))
         (_ (user-error "No shell debugger for %s" sh-shell))))
      (_ (user-error "No debugger for %s" major-mode)))))

;;;###autoload
(defun +realgud:toggle-breakpoint (&optional bang)
  "Toggle break point."
  (interactive (list evil-ex-bang)) ;; <=> `evil-define-command' with (interactive "<!>")
  (call-interactively (if bang #'realgud:cmd-clear #'realgud:cmd-break)))

(with-eval-after-load 'evil
  (evil-set-command-properties +realgud:start '(:ex-arg file))
  (evil-set-command-properties +realgud:toggle-breakpoint '(:ex-arg t)))

;; Add some missing gdb/rr commands
(defun +realgud:cmd-run (arg)
  "Run."
  (interactive "p")
  (realgud-command "run"))

(defun +realgud:cmd-start (arg)
  "start => break main; run."
  (interactive "p")
  (realgud-command "start"))

(defun +realgud:cmd-reverse-next (arg)
  "Reverse next."
  (interactive "p")
  (realgud-command "reverse-next"))

(defun +realgud:cmd-reverse-step (arg)
  "Reverse step."
  (interactive "p")
  (realgud-command "reverse-step"))

(defun +realgud:cmd-reverse-continue (arg)
  "Reverse continue."
  (interactive "p")
  (realgud-command "reverse-continue"))

(defun +realgud:cmd-reverse-finish (arg)
  "Reverse finish."
  (interactive "p")
  (realgud-command "reverse-finish"))

;;;###autoload(autoload '+realgud-hydra/body "../modules/extras/me-realgud" "Hydra keys for RealGUD." t)
(defhydra +realgud-hydra (:color pink :hint nil :foreign-keys run)
  "
[Flymake]                                                                                        [_q_] quit
 ├───────────────────────────────────────────────────────────────────────────────────────────────────────╮
 │ Stepping    [_n_] next        [_i_] step      [_o_] finish    [_c_] continue    [_R_] restart   [_u_] until-here  │
 │ Revese     [_rn_] next       [_ri_] step     [_ro_] finish   [_rc_] continue                                  │
 │ Breakpts   [_ba_] break      [_bD_] delete   [_bt_] tbreak   [_bd_] disable    [_be_] enable   [_tr_] backtrace   │
 │ Eval       [_ee_] at-point   [_er_] region   [_eE_] eval                                                    │
 │             [_!_] shell      [_Qk_] kill     [_Sg_] gdb      [_Ss_] start      [_Sr_] run                       │
 ╰───────────────────────────────────────────────────────────────────────────────────────────────────────╯
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
  ("Sr" +realgud:cmd-run)
  ("q"  nil :color blue) ;; :exit
  ("Qq" realgud:cmd-quit :color blue)) ;; :exit


(provide 'obsolete/me-realgud)
;;; me-realgud.el ends here
