;;; me-realgud.el --- Extra commands for RealGUD with better evil integration -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;; Evil integraion has been adopted from Doom Emacs
;; https://github.com/doomemacs/doomemacs/blob/master/modules/tools/debugger/autoload/evil.el

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


(with-eval-after-load 'evil
  (defun +realgud:start (&optional path)
    "Start the RealGUD debugger suitable for the current mode."
    (interactive (list (when evil-called-from-ex-p (evil-ex-file-arg)))) ;; <=> `evil-define-command' with (interactive "<f>")
    (let ((default-directory (or (+project-safe-root) default-directory)))
      (pcase major-mode
        ((or 'c-mode 'c++-mode 'c-ts-mode 'c++-ts-mode
             'objc-mode 'fortran-mode 'ada-mode 'modula-2-mode
             'd-mode 'opencl-c-mode 'go-mode 'go-ts-mode)
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

  (defun +realgud:toggle-breakpoint (&optional bang)
    "Toggle break point."
    (interactive (list evil-ex-bang)) ;; <=> `evil-define-command' with (interactive "<!>")
    (call-interactively (if bang #'realgud:cmd-clear #'realgud:cmd-break)))

  (evil-set-command-properties +realgud:start '(:ex-arg file))
  (evil-set-command-properties +realgud:toggle-breakpoint '(:ex-arg t)))

;; Add some missing gdb/rr commands
(defun +realgud:cmd-run ()
  "Run."
  (interactive)
  (realgud-command "run"))

(defun +realgud:cmd-start ()
  "start => break main; run."
  (interactive)
  (realgud-command "start"))

(defun +realgud:cmd-reverse-next ()
  "Reverse next."
  (interactive)
  (realgud-command "reverse-next"))

(defun +realgud:cmd-reverse-step ()
  "Reverse step."
  (interactive)
  (realgud-command "reverse-step"))

(defun +realgud:cmd-reverse-continue ()
  "Reverse continue."
  (interactive)
  (realgud-command "reverse-continue"))

(defun +realgud:cmd-reverse-finish ()
  "Reverse finish."
  (interactive)
  (realgud-command "reverse-finish"))


(provide 'obsolete/me-realgud)
;;; me-realgud.el ends here
