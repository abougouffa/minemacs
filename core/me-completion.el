;;; completion.el --- Completion packages -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(use-package cape
  :straight t
  :after minemacs-loaded
  :demand t
  :config
  (dolist (fn '(cape-tex ; TeX commands
                cape-file ; File names
                cape-ispell ; Words from Ispell
                cape-symbol ; Elisp symbols
                cape-keyword)) ; Keywords
    (add-to-list 'completion-at-point-functions fn)))

(use-package corfu
  :straight t
  :hook (minemacs-after-startup . global-corfu-mode)
  :init
  (add-to-list
   'load-path
   (concat
    straight-base-dir
    (format "straight/%s/corfu/extensions" straight-build-dir)))
  :custom
  (corfu-auto t) ; Enable auto completion
  (corfu-cycle t) ; Allows cycling through candidates
  (corfu-min-width 25)
  (corfu-auto-delay 0.2)
  :config
  (with-eval-after-load 'evil
    (define-key corfu-map (kbd "C-j") 'corfu-next)
    (define-key corfu-map (kbd "C-k") 'corfu-previous))

  (defun +corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (setq-local corfu-auto nil) ; Enable/disable auto completion
      (corfu-mode 1)))

  (add-hook 'minibuffer-setup-hook #'+corfu-enable-in-minibuffer))

(use-package corfu-popupinfo
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay 0.1)
  (corfu-popupinfo-max-height 15)
  :config
  (define-key corfu-map (kbd "M-p") #'corfu-popupinfo-scroll-down)
  (define-key corfu-map (kbd "M-n") #'corfu-popupinfo-scroll-up)
  (define-key corfu-map (kbd "M-d") #'corfu-popupinfo-toggle))

(use-package corfu-history
  :hook (corfu-mode . corfu-history-mode)
  :config
  (unless (bound-and-true-p savehist-mode)
    (savehist-mode 1))
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package corfu-terminal
  :straight t
  :hook (corfu-mode . corfu-terminal-mode))

(use-package kind-icon
  :straight t
  :after corfu
  :demand t
  :custom
  (kind-icon-default-style '(:padding 0
                             :stroke 0
                             :margin 0
                             :radius 0
                             :height 0.8
                             :scale 1.05)) ; Fix the scaling/height
  (kind-icon-use-icons (+emacs-features-p 'rsvg)) ; Use icons only in Emacs built with SVG support
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil) ; Use midpoint color between foreground and background colors ("blended")?
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package consult
  :straight t
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  :init
  (define-key minibuffer-local-map (kbd "C-r") 'consult-history)
  (define-key minibuffer-local-map (kbd "S-C-v") 'consult-yank-pop)
  (global-set-key (kbd "C-s") 'consult-line)
  (+map
    ;; buffer
    "bl"  #'consult-line
    "bb"  #'consult-buffer
    "bB"  #'consult-buffer-other-window
    "bF"  #'consult-buffer-other-frame
    "bmM" #'consult-bookmark
    "bI"  #'consult-imenu
    "bO"  #'consult-outline
    ;; file
    "fr"  #'consult-recent-file
    ;; git/vc
    "gG"  #'consult-git-grep
    ;; search
    "ss"  #'consult-ripgrep
    "sg"  #'consult-grep
    "sf"  #'consult-find
    "sM"  #'consult-man
    "st"  #'consult-locate
    "sh"  #'consult-history
    ;; project
    "pl"  #'consult-line-multi
    "pi"  #'consult-imenu-multi
    ;; code
    "cm"  #'consult-flymake
    "cE"  #'consult-compile-error
    "xc"  #'consult-complex-command
    ;; insert
    "iy"  #'consult-yank-from-kill-ring
    ;; help
    "hu"  #'consult-theme
    "hI"  #'consult-info)
  (+map-local :keymaps 'org-mode-map
    "h"   #'consult-org-heading)
  :config
  (setq-default completion-in-region-function #'consult-completion-in-region)

  (defun +region-or-thing-at-point ()
    "Return the region or the thing at point."
    (when-let ((thing (or (thing-at-point 'region)
                          (thing-at-point 'symbol)
                          (thing-at-point 'string)
                          (thing-at-point 'email)
                          (thing-at-point 'number)
                          (thing-at-point 'word))))
      (substring-no-properties thing)))

  ;; TWEAK Fill the `initial' query of `consult' commands from `thing-at-point'.
  ;; NOTE Some `consult' commands have slightly different signature, the
  ;; `initial' argument can come first in some cases (like `consult-line') or
  ;; second in some other cases (like `condult-grep'). These two advices are
  ;; added to such family of commands so it is filled in the right place.
  (dolist (cmd '(consult-line ; `initial' comes first in these commands
                 consult-man))
    (advice-add
     cmd :around
     (defun +consult--dwim-first-arg-a (orig-fn &optional initial opt)
       (apply orig-fn
              (append
               (if (called-interactively-p)
                   (list (or initial (+region-or-thing-at-point)))
                 (list initial))
               (when opt (list opt)))))))

  (dolist (cmd '(consult-ripgrep ; `initial' comes second in these commands
                 consult-grep
                 consult-find))
    (advice-add
     cmd :around
     (defun +consult--dwim-second-arg-a (orig-fn &optional dir initial)
       (apply orig-fn
              (append
               (list dir)
               (if (called-interactively-p)
                   (list (or initial (+region-or-thing-at-point)))
                 (list initial))))))))

(use-package embark
  :straight t
  :init
  (global-set-key [remap describe-bindings] #'embark-bindings)
  (setq prefix-help-command #'embark-prefix-help-command)
  (+map "." #'embark-act))

(use-package embark-consult
  :straight t
  :after embark consult
  :demand t)

(use-package marginalia
  :straight t
  :hook (minemacs-after-startup . marginalia-mode))

(use-package all-the-icons-completion
  :straight t
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))

(use-package orderless
  :straight t
  :after minemacs-loaded
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :straight t
  :hook (minemacs-after-startup . vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-resize nil)
  (vertico-count 12)
  :init
  (add-to-list
   'load-path (concat
               straight-base-dir
               (format "straight/%s/vertico/extensions" straight-build-dir)))
  :config
  (with-eval-after-load 'evil
    (define-key vertico-map (kbd "C-j") #'vertico-next)
    (define-key vertico-map (kbd "C-k") #'vertico-previous)))

(use-package vertico-directory
  :after vertico
  :demand t
  :config
  (define-key vertico-map "\r" #'vertico-directory-enter)
  (define-key vertico-map "\d" #'vertico-directory-delete-char)
  (define-key vertico-map "\M-\d" #'vertico-directory-delete-word)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  (with-eval-after-load 'evil
    (define-key vertico-map (kbd "M-h") #'vertico-directory-up)))

(use-package vertico-repeat
  :hook (minibuffer-setup . vertico-repeat-save))


(provide 'me-completion)
