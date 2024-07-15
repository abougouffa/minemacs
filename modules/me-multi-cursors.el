;;; me-multi-cursors.el --- Programming stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package iedit
  :straight t
  :bind ("C-;" . iedit-mode))

(use-package multiple-cursors
  :straight t
  :bind (("C->"           . mc/mark-next-like-this)
         ("C-<"           . mc/mark-previous-like-this)
         ("C-c C-<"       . mc/mark-all-like-this)
         ("C-S-c x"       . mc/mark-more-like-this-extended)
         ("C-S-c a"       . mc/mark-all-dwim)
         ("C-S-c s"       . mc/mark-next-symbol-like-this)
         ("C-S-c S"       . mc/mark-previous-like-this-symbol)
         ("C-S-c C-s"     . mc/mark-all-symbols-like-this)
         ("C-S-c C-S-c"   . mc/edit-lines)
         ("C-S-c C-e"     . mc/edit-ends-of-lines)
         ("C-S-c C-a"     . mc/edit-beginnings-of-lines)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click))
  :config
  (with-eval-after-load 'symbol-overlay
    ;; https://lmno.lol/alvaro/its-all-up-for-grabs-and-it-compounds
    (defun +mc/mark-all-symbol-overlays ()
      "Mark all symbol overlays using multiple cursors."
      (interactive)
      (mc/remove-fake-cursors)
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
        (mc/maybe-multiple-cursors-mode)))))


(provide 'me-multi-cursors)

;;; me-multi-cursors.el ends here
