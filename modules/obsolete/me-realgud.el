;;; me-realgud.el --- Extra commands for RealGUD with better evil integration -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-12-25
;; Last modified: 2025-11-06

;;; Commentary:

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
