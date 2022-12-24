;; -*- lexical-binding: t; -*-

;; Adapted from Doom Emacs
;; https://github.com/doomemacs/doomemacs/blob/master/modules/tools/debugger/autoload/evil.el

;;;###autoload (autoload '+realgud:start "../modules/extras/me-realgud" "Start the RealGUD debugger suitable for the current mode." t)
(evil-define-command +realgud:start (&optional path)
  "Start the RealGUD debugger suitable for the current mode."
  (interactive "<f>")
  (let ((default-directory
         (or (projectile-project-root)
             (and (project-current) (project-root (project-current)))
             default-directory)))
    (pcase major-mode
      ((or 'c-mode 'c++-mode 'c-ts-mode c++-ts-mode)
       (realgud:gdb (if path (concat "gdb " path))))
      ((or 'rust-mode 'rust-ts-mode)
       (realgud--lldb (if path (concat "gdb " path))))
      ((or 'js-mode 'js2-mode 'js3-mode 'typescript-mode 'js-ts-mode 'typescript-ts-mode)
       (realgud:trepanjs))
      ((or 'sh-mode 'bash-ts-mode)
       (let ((shell sh-shell))
         (when (string= shell "sh")
           (setq shell "bash"))
         (pcase shell
           ("bash"
            (realgud:bashdb (if path (concat "bashdb " path))))
           ("zsh"
            (realgud:zshdb (if path (concat "zshdb " path))))
           (_ (user-error "No shell debugger for %s" shell)))))
      (_ (user-error "No debugger for %s" major-mode)))))

;;;###autoload (autoload '+realgud:toggle-breakpoint "../modules/extras/me-realgud" "Toggle break point." t)
(evil-define-command +realgud:toggle-breakpoint (&optional bang)
  "Toggle break point."
  (interactive "<!>")
  (call-interactively (if bang #'realgud:cmd-clear #'realgud:cmd-break)))
