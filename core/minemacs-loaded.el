;; -*- lexical-binding: t; -*-

;;; Virtual module loaded at end of init.el (after custom.el)
;;; Used to synchronize loading some other stuff after loading Emacs

;; Run hooks
(when minemacs-after-startup-hook
  (run-hooks 'minemacs-after-startup-hook))

(run-with-idle-timer
 2 nil ;; 2s
 (lambda () (require 'minemacs-lazy)))


(provide 'minemacs-loaded)
