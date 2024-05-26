;;; completion.el --- Completion packages -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package cape
  :straight t
  :after minemacs-first-file
  :commands +cape-apply-capf-super +toggle-cape-auto-capf-super
  :preface
  (defcustom +cape-global-capes '(tempel-complete :completion cape-dict)
    "A list of global capes to be available at all times.
The key `:completion' is used to specify where completion candidates should be
placed, otherwise they come first."
    :group 'minemacs-completion
    :type '(repeat symbol))
  (defcustom +cape-hosts
    '(eglot-completion-at-point
      lsp-completion-at-point
      elisp-completion-at-point
      tags-completion-at-point-function)
    "A prioritised list of host capfs to create a super cape onto from
`+cape-global-capes'."
    :group 'minemacs-completion
    :type '(repeat function))
  :init
  ;; Silence the pcomplete capf, no errors or messages! Important for corfu!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  (when (< emacs-major-version 29)
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

  (+add-hook! 'completion-at-point-functions '(cape-file cape-keyword cape-dict))

  (+add-hook! (emacs-lisp-mode git-commit-mode)
    (add-hook 'completion-at-point-functions #'cape-elisp-symbol nil t))

  (+add-hook! org-mode
    (add-hook 'completion-at-point-functions #'cape-elisp-block nil t))

  (+add-hook! (TeX-mode LaTeX-mode)
    (add-hook 'completion-at-point-functions #'cape-tex nil t))
  :config
  ;; Make use of `cape''s super Capf functionality. Adapted from:
  ;; git.sr.ht/~gagbo/doom-config/tree/master/item/modules/completion/corfu/config.el
  (defun +cape-apply-capf-super ()
    "Apply Capf super to all capes specified in `+cape-global-capes' and `+cape-hosts'."
    (interactive)
    (when-let ((host (cl-intersection +cape-hosts completion-at-point-functions)))
      (setq-local
       completion-at-point-functions
       (cl-substitute (apply #'cape-capf-super
                             (cl-substitute (car host)
                                            :completion
                                            (cl-pushnew :completion +cape-global-capes)))
                      (car host)
                      completion-at-point-functions))))

  (defun +toggle-cape-auto-capf-super (&optional disable)
    "Enable auto generating Cape's super Capf.
This depends on `+cape-hosts' and `+cape-global-capes'."
    (interactive)
    (let ((enabled (get '+cape-auto-capf-super 'enabled)))
      (dolist (hook '(lsp-mode-hook eglot-managed-mode-hook change-major-mode-hook))
        (apply (if (or enabled disable) #'remove-hook #'add-hook) (list hook #'+cape-apply-capf-super))
        (put '+cape-auto-capf-super 'enabled (not (or enabled disable)))))))

(use-package corfu
  :straight (:files (:defaults "extensions/*.el"))
  :hook (eshell-mode . +corfu-less-intrusive-h)
  :hook (minibuffer-setup . +corfu-enable-in-minibuffer-h)
  :bind (:map corfu-map
         ("M-m" . +corfu-complete-in-minibuffer)
         ("<tab>" . corfu-next)
         ("<backtab>" . corfu-previous)
         ("C-j" . corfu-next)
         ("C-k" . corfu-previous))
  :custom
  (corfu-auto t) ; Enable auto completion
  (corfu-cycle t) ; Allows cycling through candidates
  (corfu-min-width 25)
  (corfu-auto-delay 0.2)
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
      (apply #'consult-completion-in-region completion-in-region--data))))

(use-package corfu-popupinfo
  :hook (corfu-mode . corfu-popupinfo-mode)
  :bind (:package corfu
         :map corfu-map
         ("M-p" . corfu-popupinfo-scroll-down)
         ("M-n" . corfu-popupinfo-scroll-up)
         ("M-d" . corfu-popupinfo-toggle))
  :custom
  (corfu-popupinfo-delay 0.1)
  (corfu-popupinfo-max-height 15))

(use-package corfu-history
  :hook (corfu-mode . corfu-history-mode)
  :config
  (unless (bound-and-true-p savehist-mode)
    (savehist-mode 1))
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package corfu-terminal
  :straight t
  :hook (corfu-mode . corfu-terminal-mode))

(use-package nerd-icons-corfu
  :straight t
  :after corfu
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
  ;; In the minibuffer, "C-k" is be mapped to act like "<up>". However, in
  ;; Emacs, "C-k" have a special meaning of `kill-line'. So lets map "C-S-k"
  ;; to serve the original "C-k".
  :bind (:map vertico-map
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous)
         :map minibuffer-local-map
         ("C-S-k" . kill-line))
  :custom
  (vertico-cycle t)
  (vertico-resize nil)
  (vertico-count 12))

(use-package vertico-directory
  :after vertico
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :bind (:map vertico-map
         ("RET" . vertico-directory-enter)
         ("DEL" . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word)
         ("M-h" . vertico-directory-up)))

(use-package vertico-repeat
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind ("M-R" . vertico-repeat))

(use-package vertico-mouse
  :hook (vertico-mode . vertico-mouse-mode))


(provide 'me-completion)

;;; me-completion.el ends here
