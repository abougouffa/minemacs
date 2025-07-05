;;; me-smerge-x.el --- Exta functionalities for `smerge' -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-07-05
;; Last modified: 2025-07-05

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

(defun +smerge-vc-next-conflict-recenter ()
  "Like `smerge-vc-next-conflict' but recenters the buffer."
  (interactive)
  (smerge-vc-next-conflict)
  ;; Often, after calling `smerge-vc-next-conflict', the cursor will land at
  ;; the bottom of the window.
  (recenter-top-bottom (/ (window-height) 8)))

(defun +smerge-next-recenter ()
  "Like `smerge-next' but recenters the buffer."
  (interactive)
  (smerge-next)
  (recenter-top-bottom (/ (window-height) 8)))

(defun +smerge-prev-recenter ()
  "Like `smerge-prev' but recenters the buffer."
  (interactive)
  (smerge-prev)
  (recenter-top-bottom (/ (window-height) 8)))


(provide 'me-smerge-x)
;;; me-smerge-x.el ends here
