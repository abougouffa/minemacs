;;; me-multi-cursors.el --- Programming stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

;; Modify multiple occurrences simultaneously
(use-package iedit
  :straight t
  :bind ("C-;" . iedit-mode))


;; Multiple cursors implementation for Emacs
(use-package multiple-cursors
  :straight t
  :bind (("C->"           . mc/mark-next-like-this)
         ("C-<"           . mc/mark-previous-like-this)
         ("C-M->"         . mc/skip-to-next-like-this)
         ("C-M-<"         . mc/skip-to-previous-like-this)
         ("C-c C-<"       . mc/mark-all-like-this)
         ("C-S-c x"       . mc/mark-more-like-this-extended)
         ("C-S-c a"       . mc/mark-all-dwim)
         ("C-S-c s"       . mc/mark-next-symbol-like-this)
         ("C-S-c S"       . mc/mark-previous-like-this-symbol)
         ("C-S-c C-s"     . mc/mark-all-symbols-like-this)
         ("C-S-c C-S-c"   . mc/edit-lines)
         ("C-S-c C-e"     . mc/edit-ends-of-lines)
         ("C-S-c C-a"     . mc/edit-beginnings-of-lines)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         ("C-S-c SPC"     . +mc/transient))
  :custom
  (mc/list-file (concat minemacs-local-dir "mc-list.el"))
  :commands (+mc/transient)
  :config
  ;; Add some extra commands to be run on all cursors
  (cl-callf append mc--default-cmds-to-run-for-all
    '(;; Some extra Emacs commands
      beginning-of-visual-line end-of-visual-line kill-region forward-sexp backward-sexp
      tab-to-tab-stop indent-for-tab-command transient-noop comment-line comment-dwim
      ;; MinEmacs' commands
      +kill-whitespace-or-word +kill-region-or-backward-word +backward-kill-whitespace-or-word
      ;; `avy'
      avy-goto-char avy-goto-char-timer avy-goto-char-in-line avy-goto-char-2
      ;; `avy-zap'
      avy-zap-to-char avy-zap-up-to-char avy-zap-to-char-dwim avy-zap-up-to-char-dwim
      ;; `crux'
      crux-smart-kill-line crux-smart-open-line crux-smart-open-line-above
      ;; `expreg'
      expreg-expand expreg-contract
      ;; Org specific commands
      org-delete-char org-self-insert-command))

  (cl-callf append mc--default-cmds-to-run-once
    '(pixel-scroll-precision +mc/mark-all-symbol-overlays))

  (with-eval-after-load 'transient
    (transient-define-prefix +mc/transient ()
      "Multiple-cursors transient menu."
      [["Up"
        ("p" "prev" mc/mark-previous-like-this :transient t)
        ("P" "skip" mc/skip-to-previous-like-this :transient t)
        ("M-p" "unmark" mc/unmark-previous-like-this :transient t)
        ("|" "align with input CHAR" mc/vertical-align :transient t)]
       ["Down"
        ("n" "next" mc/mark-next-like-this :transient t)
        ("N" "skip" mc/skip-to-next-like-this :transient t)
        ("M-n" "unmark" mc/unmark-next-like-this :transient t)]
       ["Misc"
        ("l" "edit lines" mc/edit-lines)
        ("a" "mark all" mc/mark-all-like-this)
        ("s" "search" mc/mark-all-in-region-regexp)
        ("<mouse-1>" "click" mc/add-cursor-on-click :transient t)]
       ["Insert"
        ("0" "insert numbers" mc/insert-numbers)
        ("A" "insert letters" mc/insert-letters)]]))

  ;; Integrate with `symbol-overlay'
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
        (mc/maybe-multiple-cursors-mode)))

    (with-eval-after-load 'transient
      ;; Add to the transient menu after the "s"
      (transient-append-suffix '+mc/transient "s" '("S" "symbol overlays" +mc/mark-all-symbol-overlays)))))


(provide 'me-multi-cursors)

;;; me-multi-cursors.el ends here
