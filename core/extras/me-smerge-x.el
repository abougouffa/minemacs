;;; me-smerge-x.el --- Exta functionalities for `smerge' -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-07-05
;; Last modified: 2025-08-20

;;; Commentary:

;;; Code:

;;;###autoload(with-eval-after-load 'smerge-mode (require 'me-smerge-x))

(defun +smerge-first ()
  "Got to the first occurrence."
  (interactive)
  (goto-char (point-min))
  (smerge-next))

(defun +smerge-last ()
  "Got to the last occurrence."
  (interactive)
  (goto-char (point-max))
  (smerge-prev))

;; Often, after calling `smerge-next', the cursor will land at the bottom of the
;; window.
(defun +recenter:after-a (&rest _args) (recenter-top-bottom (/ (window-height) 8)))
(dolist (cmd '(smerge-next smerge-prev smerge-vc-next-conflict))
  (advice-add cmd :after #'+recenter:after-a))


(provide 'me-smerge-x)
;;; me-smerge-x.el ends here
