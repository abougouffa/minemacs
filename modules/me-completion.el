;;; completion.el --- Completion packages -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package cape
  :straight t
  :after minemacs-first-file
  :init
  ;; Silence the pcomplete capf, no errors or messages! Important for corfu!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Make these capfs composable
  (advice-add 'comint-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive)

  (when (< emacs-major-version 29)
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

  (+add-hook! 'completion-at-point-functions '(cape-file cape-keyword cape-dict))

  (+add-hook! (emacs-lisp-mode git-commit-mode)
    (add-hook 'completion-at-point-functions #'cape-elisp-symbol nil t))

  (+add-hook! org-mode
    (add-hook 'completion-at-point-functions #'cape-elisp-block nil t))

  (+add-hook! (TeX-mode LaTeX-mode)
    (add-hook 'completion-at-point-functions #'cape-tex nil t)))

(use-package corfu
  :straight (:files (:defaults "extensions/*.el"))
  :hook (eshell-mode . +corfu-less-intrusive-h)
  :hook (minibuffer-setup . +corfu-enable-in-minibuffer-h)
  :hook (corfu-mode . corfu-popupinfo-mode)
  :hook (corfu-mode . corfu-history-mode)
  :bind (;; JK for movement
         :map corfu-map
         ("M-m" . +corfu-complete-in-minibuffer)
         ("<tab>" . corfu-next)
         ("<backtab>" . corfu-previous)
         ("C-j" . corfu-next)
         ("C-k" . corfu-previous)
         ;; For `corfu-popupinfo'
         ("M-p" . corfu-popupinfo-scroll-down)
         ("M-n" . corfu-popupinfo-scroll-up)
         ("M-d" . corfu-popupinfo-toggle))
  :custom
  (corfu-auto t) ; Enable auto completion
  (corfu-cycle t) ; Allows cycling through candidates
  (corfu-min-width 25)
  (corfu-auto-delay 0.2)
  (corfu-popupinfo-delay 0.1)
  (corfu-popupinfo-max-height 15)
  :init
  (+hook-once! prog-mode-hook (global-corfu-mode 1))
  :config
  (defun +corfu-enable-in-minibuffer-h ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (setq-local corfu-auto nil) ; Enable/disable auto completion
      (corfu-mode 1)))

  (defun +corfu-less-intrusive-h ()
    (setq-local corfu-quit-at-boundary t
                corfu-quit-no-match t
                corfu-auto nil)
    (corfu-mode 1))

  ;; Taken from:
  ;; git.sr.ht/~gagbo/doom-config/tree/master/item/modules/completion/corfu/config.el
  (defun +corfu-complete-in-minibuffer ()
    "Move current completions to the minibuffer."
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold
          completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))

  ;; Ensure `savehist-mode' is on add `corfu-history' to the saved variables
  (unless (bound-and-true-p savehist-mode)
    (savehist-mode 1))
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package corfu-terminal
  :straight t
  :hook (corfu-mode . corfu-terminal-mode))

(use-package nerd-icons-corfu
  :straight t
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package consult
  :straight t
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  :bind (:map minibuffer-local-map
         ("C-r" . consult-history)
         ("C-S-v" . consult-yank-pop)
         :package isearch
         :map isearch-mode-map
         ("C-S-v" . consult-yank-pop))
  :custom
  ;; Use `consult-xref' for `xref-find-references'
  (xref-show-xrefs-function #'consult-xref)
  ;; Better formatting for `view-register'
  (register-preview-function #'consult-register-format)
  :init
  (+map!
    ;; buffer
    "bll" #'consult-line
    "blf" #'consult-focus-lines
    "blk" #'consult-keep-lines
    "blg" #'consult-goto-line
    "bb"  #'consult-buffer
    "bB"  #'consult-buffer-other-window
    "bF"  #'consult-buffer-other-frame
    "bmM" #'consult-bookmark
    "bi"  #'consult-imenu
    "bO"  #'consult-outline
    ;; file
    "fr"  #'consult-recent-file
    ;; git/vc
    "gG"  #'consult-git-grep
    ;; search
    "ss"  (if (executable-find "rg") #'consult-ripgrep #'consult-grep)
    "sS"  (if (executable-find "rg") #'consult-grep #'consult-ripgrep)
    "sf"  (if (executable-find "fd") #'consult-fd #'consult-find)
    "sF"  (if (executable-find "fd") #'consult-find #'consult-fd)
    "sM"  #'consult-man
    "st"  #'consult-locate
    "sh"  #'consult-history
    "sa"  #'consult-org-agenda
    "sl"  #'consult-locate
    "si"  #'consult-isearch-history
    ;; project
    "pl"  #'consult-line-multi
    "pi"  #'consult-imenu-multi
    ;; code
    "cm"  #'consult-flymake
    "cE"  #'consult-compile-error
    ;; extras
    "ec"  #'consult-complex-command
    ;; insert
    "iy"  #'consult-yank-from-kill-ring
    "ip"  #'consult-yank-pop
    "ir"  '(nil :wk "register")
    "irr" #'consult-register
    "irl" #'consult-register-load
    "irs" #'consult-register-store
    ;; help
    "hu"  #'consult-theme
    "hI"  #'consult-info)
  (+map-local! :keymaps 'org-mode-map
    "h"   #'consult-org-heading)
  :config
  (setq-default completion-in-region-function #'consult-completion-in-region)

  ;; Fill the initial query of `consult' commands from region or thing at point.
  (consult-customize
   consult-find :initial (+region-or-thing-at-point)
   consult-grep :initial (+region-or-thing-at-point)
   consult-line :initial (+region-or-thing-at-point)
   consult-line-multi :initial (+region-or-thing-at-point)
   consult-man :initial (+region-or-thing-at-point)
   consult-ripgrep :initial (+region-or-thing-at-point)))

(use-package consult-dir
  :straight t
  :bind (("C-x C-d" . consult-dir)
         :package vertico
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :init
  (+map! "ed" #'consult-dir))

(use-package embark
  :straight t
  :bind (("<remap> <describe-bindings>" . embark-bindings)
         ("C-²" . embark-act) ; In a French AZERTY keyboard, the ² key is right above TAB
         ("M-²" . embark-collect)
         ("C-&" . embark-dwim))
  :init
  ;; Use Embark to show bindings in a key prefix with `C-h`
  (setq prefix-help-command #'embark-prefix-help-command)
  (+map!
    "a" #'embark-act
    "A" #'embark-collect))

(use-package embark-consult
  :straight t
  :after embark consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  :straight t
  :hook (minemacs-lazy . marginalia-mode))

(use-package nerd-icons-completion
  :straight t
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :straight (:files (:defaults "extensions/*.el"))
  :hook (minemacs-lazy . vertico-mode)
  :hook (minemacs-lazy . vertico-mouse-mode)
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :hook (minibuffer-setup . vertico-repeat-save)
  ;; In the minibuffer, "C-k" is be mapped to act like "<up>". However, in
  ;; Emacs, "C-k" have a special meaning of `kill-line'. So lets map "C-S-k"
  ;; to serve the original "C-k".
  :bind (("M-R" . vertico-repeat)
         :map vertico-map
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous)
         ("RET" . vertico-directory-enter)
         ("DEL" . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word)
         ("M-h" . vertico-directory-up)
         :map minibuffer-local-map
         ("C-S-k" . kill-line))
  :custom
  (vertico-cycle t)
  (vertico-resize nil)
  (vertico-count 12))


(provide 'me-completion)

;;; me-completion.el ends here
