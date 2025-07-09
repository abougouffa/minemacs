;;; me-symbol-overlay.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-07-09
;; Last modified: 2025-07-09

;;; Commentary:

;;; Code:


;; Highlight symbols with keymap-enabled overlays
(use-package symbol-overlay
  :straight t
  :config
  (with-eval-after-load 'multiple-cursors
    ;; https://lmno.lol/alvaro/its-all-up-for-grabs-and-it-compounds
    (defun +mc/mark-all-symbol-overlays (&optional discard)
      "Mark all symbol overlays using multiple cursors.
When DISCARD is non-nil, discard the current cursors before creating the
new ones."
      (interactive "P")
      (when discard (mc/remove-fake-cursors))
      (when-let* ((overlays (symbol-overlay-get-list 0))
                  (point (point))
                  (point-overlay (seq-find
                                  (lambda (overlay)
                                    (and (<= (overlay-start overlay) point)
                                         (<= point (overlay-end overlay))))
                                  overlays))
                  (offset (- point (overlay-start point-overlay))))
        (setq deactivate-mark t)
        (mapc (lambda (overlay)
                (unless (eq overlay point-overlay)
                  (mc/save-excursion
                   (goto-char (+ (overlay-start overlay) offset))
                   (mc/create-fake-cursor-at-point))))
              overlays)
        (mc/maybe-multiple-cursors-mode)))

    (with-eval-after-load 'transient
      ;; Add to the transient menu after the "s"
      (transient-append-suffix '+mc/transient "s" '("S" "symbol overlays" +mc/mark-all-symbol-overlays)))

    (with-eval-after-load 'casual-symbol-overlay
      (transient-append-suffix 'casual-symbol-overlay-tmenu '(-2)
        ["Multiple cursors" ("c" "Mark all" +mc/mark-all-symbol-overlays)]))))


;; An opinionated `transient' menu for `symbol-overlay'
(use-package casual-symbol-overlay
  :straight t
  :bind (:package symbol-overlay :map symbol-overlay-map ("C-o" . casual-symbol-overlay-tmenu)))


(provide 'obsolete/me-symbol-overlay)
;;; me-symbol-overlay.el ends here
