;;; me-editor.el --- Editing stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-09-17
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;; Visualize and navigate the undo tree
(use-package vundo
  :ensure t
  :bind (:map minemacs-open-thing-map ("u" . vundo))
  :commands (+vundo-diff-mode)
  :custom
  (vundo-compact-display t)
  (vundo-window-max-height 8)
  (vundo-glyph-alist vundo-unicode-symbols)
  :config
  (defun +vundo--diff-wrapper (&rest _) (vundo-diff))
  (defcustom +vundo-diff-commands
    '(vundo-next vundo-forward vundo-stem-root vundo-stem-end vundo-goto-last-saved vundo-goto-next-saved)
    "A list of commands after which `vundo-diff' gets called."
    :group 'minemacs-editor
    :type '(repeat function))
  (define-minor-mode +vundo-diff-mode
    "Automatically show diffs when navigating the undo tree."
    :global t
    (if +vundo-diff-mode
        (satch-advice-add +vundo-diff-commands :after #'+vundo--diff-wrapper)
      (satch-advice-remove +vundo-diff-commands #'+vundo--diff-wrapper)))
  (+vundo-diff-mode 1))


;; Persistent undo tree between sessions
(use-package undo-fu-session
  :ensure t
  :hook (minemacs-lazy . undo-fu-session-global-mode)
  :custom
  (undo-fu-session-compression (if (executable-find "zstd") 'zst 'gz)))


;; Modify multiple occurrences simultaneously
(use-package iedit
  :ensure t
  :bind (("C-;" . iedit-mode)
         ("C-x r ;" . iedit-rectangle-mode)
         (:map esc-map ("C-;" . iedit-execute-last-modification)))
  :bind (:package isearch :map isearch-mode-map ("C-;" . iedit-mode-from-isearch))
  :custom
  (iedit-auto-save-occurrence-in-kill-ring nil))


;; Multiple cursors implementation for Emacs
(use-package multiple-cursors
  :ensure t
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
  (keymap-unset mc/keymap "<return>" t) ; default `multiple-cursors-mode', I want to be able to insert newlines at fake cursors!
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


;; Unobtrusively trim extraneous white-space *ONLY* in lines edited
(use-package ws-butler
  :ensure t
  :hook (minemacs-first-file . ws-butler-global-mode)
  :config
  ;; Don't delete trailing whitespaces in `diff-mode', it is part of the syntax
  (add-to-list 'ws-butler-global-exempt-modes 'diff-mode))


;; Smart guessing the indentation offset originally used in the opened source files
(use-package dtrt-indent
  :ensure t
  :after minemacs-first-file
  :hook ((change-major-mode-after-body read-only-mode) . +dtrt-indent-mode-maybe)
  :commands (+dtrt-indent-tab-to-tab-stop)
  :bind (([remap tab-to-tab-stop] . +dtrt-indent-tab-to-tab-stop)) ; Smarter M-i
  :custom
  (dtrt-indent-max-lines 2500) ; Faster than the default 5000
  (dtrt-indent-run-after-smie t)
  :init
  (defvar +dtrt-indent-context-lines 20 "The number of lines above and below point to consider in `+dtrt-indent-tab-to-tab-stop'.")
  ;; Better predicates for enabling `dtrt-indent', inspired by Doom Emacs
  (defvar +dtrt-indent-excluded-modes
    '( emacs-lisp-mode lisp-mode pascal-mode coq-mode ; see doomemacs/doomemacs#5823
       org-mode) ; a non-standard `tab-width' causes an error in `org-mode'.
    "A list of major modes where indentation shouldn't be auto-detected.")
  (defun +dtrt-indent-mode-maybe ()
    (unless (or (not (featurep 'minemacs-first-file))
                (memq major-mode '(fundamental-mode guard-lf-large-file-mode so-long-mode))
                (member (substring (buffer-name) 0 1) '(" " "*"))
                (derived-mode-p +dtrt-indent-excluded-modes))
      ;; Don't display messages in the echo area, but still log them
      (let ((inhibit-message (not minemacs-verbose-p)))
        (dtrt-indent-mode 1))))
  :config
  ;; Add support for `protobuf-ts-mode'
  (add-to-list 'dtrt-indent-hook-mapping-list '(protobuf-ts-mode c/c++/java protobuf-ts-mode-indent-offset))
  (defun +dtrt-indent-tab-to-tab-stop ()
    "Like `tab-to-tab-stop', but set `indent-tabs-mode' according the context.
In some dirty files, there is a mix of spaces and tabs. This uses
`dtrt-indent' to detect which one to insert at point."
    (interactive)
    (let* ((lang (cadr (dtrt-indent--search-hook-mapping major-mode)))
           (result (and lang
                        (save-excursion
                          (save-restriction
                            (narrow-to-region (line-beginning-position (- +dtrt-indent-context-lines))
                                              (line-end-position +dtrt-indent-context-lines))
                            (dtrt-indent--analyze (dtrt-indent--calc-histogram lang))))))
           (modify-indentation (and (not (cdr (assoc :rejected result))) (cdr (assoc :change-indent-tabs-mode result))))
           (new-indent-tabs-mode (cdr (assoc :indent-tabs-mode-setting result))))
      (when (and modify-indentation (not (eq indent-tabs-mode new-indent-tabs-mode)))
        (+log! "Temporary changing `indent-tabs-mode' to %S for context-aware indentation" new-indent-tabs-mode))
      (let ((indent-tabs-mode (if modify-indentation new-indent-tabs-mode indent-tabs-mode)))
        (call-interactively #'tab-to-tab-stop)))))


;; Structured editing and navigation in Emacs with Tree-Sitter
(use-package combobulate-setup
  :vc (combobulate :url "https://github.com/mickeynp/combobulate" :ignored-files ("combobulate.el" "tests"))
  :when (and (not (featurep 'os/win)) (featurep 'feat/tree-sitter)) ; TEMP: disable on Windows
  :custom
  (combobulate-key-prefix "C-c b") ; "C-c o" is used by `minemacs-open-thing-map'
  :config
  (with-eval-after-load 'combobulate-query (keymap-unset combobulate-query-mode-map "q"))
  ;; TEMP+BUGFIX: Basically, load the same features that would be loaded by "combobulate.el"
  (dolist (feature '(combobulate-rules
                     combobulate-procedure combobulate-navigation
                     combobulate-manipulation combobulate-envelope combobulate-display
                     combobulate-ui combobulate-misc combobulate-query combobulate-cursor
                     combobulate-toml combobulate-html combobulate-python combobulate-js-ts
                     combobulate-css combobulate-yaml combobulate-json combobulate-go))
    (require feature))

  ;; The "M-<up/down/left/right>" keys are used globally by `move-dup', lets
  ;; unset them for `combobulate' and use "M-S-<up/down/left/right>" instead.
  (mapc (lambda (k) (keymap-unset combobulate-key-map k 'remove)) '("M-<up>" "M-<down>" "M-<left>" "M-<right>"))
  (keymap-set combobulate-key-map "M-S-<up>" #'combobulate-splice-up)
  (keymap-set combobulate-key-map "M-S-<down>" #'combobulate-splice-down)
  (keymap-set combobulate-key-map "M-S-<left>" #'combobulate-splice-self)
  (keymap-set combobulate-key-map "M-S-<right>" #'combobulate-splice-parent))


;; Parse and respect Vim modeline options (`tab-width', `fill-column', etc.)
(use-package vim-file-locals
  :vc (:url "https://github.com/abougouffa/emacs-vim-file-locals")
  :hook (minemacs-first-file . vim-file-locals-mode))


;; An Emacs minor mode for highlighting matches to the selection
(use-package selection-highlight-mode
  :vc (:url "https://github.com/abougouffa/selection-highlight-mode")
  :hook (minemacs-lazy . selection-highlight-mode)
  :init
  ;; Automatically set the face for `selection-highlight-mode'
  (satch-add-hook
   '(enable-theme-functions disable-theme-functions server-after-make-frame-hook)
   (satch-defun +selection-highlight--set-face-h (&rest _args)
     (with-eval-after-load 'selection-highlight-mode
       (with-eval-after-load 'isearch
         (when (display-graphic-p)
           (when-let* ((new-color (+color-subtle 'isearch 20)))
             (set-face-background 'selection-highlight-mode-match-face new-color)
             (set-face-background 'selection-highlight-mode-alternate-match-face new-color))))))))


;; Your friendly neighborhood expand-region clone
(use-package expreg
  :ensure t
  :when (featurep 'feat/tree-sitter)
  :bind (("C-M-SPC" . expreg-expand) ; orig. `mark-sexp'
         ("S-C-M-SPC" . expreg-contract)))


;; Eclipse-like moving and duplicating lines or rectangles
(use-package move-dup
  :ensure t
  :bind (("M-<up>" . move-dup-move-lines-up)
         ("M-<down>" . move-dup-move-lines-down)
         ("C-M-<up>" . move-dup-duplicate-up)
         ("C-M-<down>" . move-dup-duplicate-down)))


;; Perform a backup on each file save, real backup for Emacs!
(use-package real-backup
  :vc (:url "https://github.com/abougouffa/real-backup")
  :hook (minemacs-first-file . real-backup-mode))


;; Copy&paste GUI clipboard from text terminal
(use-package xclip
  :ensure t
  :hook (tty-setup . +xclip--enable-in-tty-h)
  :config
  (defun +xclip--enable-in-tty-h ()
    (let ((inhibit-message t))
      (with-demoted-errors "%s" (xclip-mode 1)))))


(provide 'me-editor)

;;; me-editor.el ends here
