;; -*- lexical-binding: t; -*-

;;; Virtual module loaded when idle after minemacs-loaded
;;; Used to synchronize loading some other stuff after loading Emacs


;; Run hooks
(when minemacs-before-user-config-hook
  (run-hooks 'minemacs-before-user-config-hook))


(provide 'minemacs-before-user-config)
