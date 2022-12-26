;; -*- lexical-binding: t; -*-

;;; Virtual module loaded when idle after minemacs-loaded
;;; Used to synchronize loading some other stuff after loading Emacs


(when (length> minemacs-lazy-hook 0)
  (if (bound-and-true-p minemacs-not-lazy)
      (progn ;; If minemacs-no-lazy is bound and true, force loading lazy hooks immediately
        (setq minemacs-lazy-hook (reverse minemacs-lazy-hook))
        (run-hooks 'minemacs-lazy-hook))
    (apply #'+eval-when-idle ; run hooks one by one, as a FIFO
           (append '(1) (reverse minemacs-lazy-hook)))))


(provide 'minemacs-lazy)
