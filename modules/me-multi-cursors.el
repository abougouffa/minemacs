;;; me-multi-cursors.el --- Programming stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")
;; Created: 2022-09-20
;; Last modified: 2025-07-23

;;; Commentary:

;;; Code:

;; Modify multiple occurrences simultaneously
(use-package iedit
  :straight t
  :bind ("C-;" . iedit-mode))


;; Multiple cursors implementation for Emacs
(use-package multiple-cursors
  :straight t
  :after minemacs-first-file
  :demand ; Otherwise, the `multiple-cursors' will not be loaded (the commands are defined in `multiple-cursors-core')
  :bind (("C->"           . mc/mark-next-like-this)
         ("C-<"           . mc/mark-previous-like-this)
         ("C-M->"         . mc/skip-to-next-like-this)
         ("C-M-<"         . mc/skip-to-previous-like-this)
         ("C-c C-<"       . mc/mark-all-like-this)
         ("C-S-c x"       . mc/mark-more-like-this-extended)
         ("C-S-c a"       . mc/mark-all-dwim)
         ("C-S-c n"       . mc/mark-next-like-this-symbol)
         ("C-S-c p"       . mc/mark-previous-like-this-symbol)
         ("C-S-c C-s"     . mc/mark-all-symbols-like-this)
         ("C-S-c C-S-c"   . mc/edit-lines)
         ("C-S-c C-e"     . mc/edit-ends-of-lines)
         ("C-S-c C-a"     . mc/edit-beginnings-of-lines)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click))
  :custom
  (mc/list-file (concat minemacs-local-dir "mc-list.el"))
  :config
  ;; Add some extra commands to be run on all cursors
  (cl-callf append mc--default-cmds-to-run-for-all
    '(;; Some extra Emacs commands
      beginning-of-visual-line end-of-visual-line kill-region forward-sexp backward-sexp
      tab-to-tab-stop indent-for-tab-command comment-line comment-dwim
      ;; MinEmacs' commands
      +kill-whitespace-or-word +kill-region-or-backward-word +backward-kill-whitespace-or-word
      +dtrt-indent-tab-to-tab-stop
      ;; `avy'
      avy-goto-char avy-goto-char-timer avy-goto-char-in-line avy-goto-char-2
      ;; `avy-zap'
      avy-zap-to-char avy-zap-up-to-char avy-zap-to-char-dwim avy-zap-up-to-char-dwim
      ;; `crux'
      crux-smart-kill-line crux-smart-open-line crux-smart-open-line-above
      ;; `expreg'
      expreg-expand expreg-contract
      ;; `org'
      org-delete-char org-self-insert-command org-force-self-insert org-return-and-maybe-indent))

  (cl-callf append mc--default-cmds-to-run-once
    '(;; Some extra Emacs commands
      pixel-scroll-precision beginning-of-buffer end-of-buffer transient-noop
      ;; `iedit'
      iedit-switch-to-mc-mode)))


(provide 'me-multi-cursors)

;;; me-multi-cursors.el ends here
