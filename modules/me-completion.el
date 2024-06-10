;;; completion.el --- Completion packages -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package cape
  :straight t
  :after minemacs-first-file
  :bind (("C-c p p" . completion-at-point) ; capf
         ("C-c p t" . complete-tag) ; etags
         ("C-c p d" . cape-dabbrev) ; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p e" . cape-elisp-block)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p :" . cape-emoji)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
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
  :bind ( ; Bidn some useful commands
         :map corfu-map
         ("M-m" . +corfu-complete-in-minibuffer)
         ("<tab>" . corfu-next)
         ("<backtab>" . corfu-previous)
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
  :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package consult
  :straight t
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c k"   . consult-kmacro)
         ("C-c h"   . consult-history)
         ("C-c r"   . consult-ripgrep)
         ("C-c c t" . consult-theme)
         ("C-c i"   . consult-info)
         ([remap Info-search] . consult-info)
         ([remap recentf-open-files] . consult-recent-file)
         ([remap recentf] . consult-recent-file)

         ;; C-x bindings in `ctl-x-map'
         ([remap repeat-complex-command] . consult-complex-command) ; C-x M-:
         ([remap switch-to-buffer] . consult-buffer) ; C-x b
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window) ; C-x 4 b
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame) ; C-x 5 b
         ([remap switch-to-buffer-other-tab] . consult-buffer-other-tab) ; C-x t b
         ([remap bookmark-jump] . consult-bookmark) ; C-x r b
         ([remap project-switch-to-buffer] . consult-project-buffer) ; C-x p b

         ;; Custom M-# bindings for fast register access
         ("M-#"     . consult-register-load)
         ("M-'"     . consult-register-store) ; overwrite `abbrev-prefix-mark' (unrelated)
         ("C-M-#"   . consult-register)

         ;; Other custom bindings
         ([remap yank-pop] . consult-yank-pop) ; M-y

         ;; M-g bindings in `goto-map'
         ("M-g e"   . consult-compile-error)
         ("M-g f"   . consult-flymake) ; Alternative: consult-flycheck
         ("M-g o"   . consult-outline) ; Alternative: consult-org-heading
         ("M-g m"   . consult-mark)
         ("M-g k"   . consult-global-mark)
         ("M-g I"   . consult-imenu-multi)
         ([remap imenu] . consult-imenu)
         ([remap goto-line] . consult-goto-line) ; M-g g or M-g M-g

         ;; M-s bindings in `search-map'
         ("M-s d"   . consult-find)
         ("M-s D"   . consult-locate)
         ("M-s g"   . consult-grep)
         ("M-s G"   . consult-git-grep)
         ("M-s r"   . consult-ripgrep)
         ("M-s l"   . consult-line)
         ("M-s L"   . consult-line-multi)
         ("M-s k"   . consult-keep-lines)
         ("M-s u"   . consult-focus-lines)

         ;; Isearch integration
         ("M-s e"   . consult-isearch-history)

         :map isearch-mode-map
         ("M-e"     . consult-isearch-history) ; orig. isearch-edit-string
         ("M-s e"   . consult-isearch-history) ; orig. isearch-edit-string
         ("M-s l"   . consult-line) ; needed by consult-line to detect isearch
         ("M-s L"   . consult-line-multi) ; needed by consult-line to detect isearch

         ;; Minibuffer history
         :map minibuffer-local-map
         ("C-s" . +consult-insert-thing-at-point)
         ([remap next-matching-history-element] . consult-history) ; M-s
         ([remap previous-matching-history-element] . consult-history)) ; M-r
  :custom
  ;; Use `consult-xref' for `xref-find-references' and `xref-find-definitions'
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (register-preview-function #'consult-register-format) ; Better formatting for `view-register'
  (consult-narrow-key "<")
  :config
  (defun +consult-insert-thing-at-point ()
    "Insert region or symbol in the minibuffer."
    (interactive)
    (insert (with-current-buffer (window-buffer (minibuffer-selected-window))
              (or (+region-or-thing-at-point t) ""))))

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
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package embark
  :straight t
  :bind (([remap describe-bindings] . embark-bindings)
         ("C-²" . embark-act) ; In a French AZERTY keyboard, the ² key is right above TAB
         ("M-²" . embark-collect)
         ("C-&" . embark-dwim))
  :init
  ;; Use Embark to show bindings in a key prefix with `C-h`
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :straight t)

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
  :bind (("M-R" . vertico-repeat)
         :map vertico-map
         ("RET" . vertico-directory-enter)
         ("DEL" . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word)
         ("M-h" . vertico-directory-up))
  :custom
  (vertico-cycle t)
  (vertico-resize nil)
  (vertico-count 12))


(provide 'me-completion)

;;; me-completion.el ends here
