;; -*- lexical-binding: t; -*-

;;; Virtual module loaded at end of init.el (after custom.el)
;;; Used to synchronize loading some other stuff after loading Emacs

;; Maybe useful
(setq minemacs-loaded-stage-1 t)

(run-at-time
 (* 2 60) ;; after 2 min
 nil
 (lambda ()
   (require 'minemacs-loaded-stage-2)))

(provide 'minemacs-loaded-stage-1)
