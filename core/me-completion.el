;;; completion.el --- Completion packages -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(use-package cape
  :straight t
  :after minemacs-loaded
  :config
  (add-to-list 'completion-at-point-functions #'cape-file) ;; complete file names
  (add-to-list 'completion-at-point-functions #'cape-tex) ;; complete TeX commands
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

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
      (setq-local corfu-auto nil) ;; Enable/disable auto completion
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
    (savehist-mode +1))
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package kind-icon
  :straight t
  :after corfu
  :custom
  ;; Fix the scaling/height
  (kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.8 :scale 1.05))
  (kind-icon-use-icons (+emacs-features-p 'rsvg)) ; Only on Emacs built with SVG support
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil) ; Use midpoint color between foreground and background colors ("blended")?
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package embark
  :straight t
  :init
  (global-set-key [remap describe-bindings] #'embark-bindings)
  (setq prefix-help-command #'embark-prefix-help-command)
  :general
  (+map "." #'embark-act))

(use-package embark-consult
  :straight t
  :after embark consult)

(use-package all-the-icons-completion
  :straight t
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))

(use-package marginalia
  :straight t
  :hook (minemacs-after-startup . marginalia-mode))

(use-package orderless
  :straight t
  :after minemacs-loaded
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
  :custom
  (vertico-buffer-display-action
   `(display-buffer-at-bottom
     (window-height . ,(+ 3 vertico-count))))
  :config
  (define-key vertico-map "\r" #'vertico-directory-enter)
  (define-key vertico-map "\d" #'vertico-directory-delete-char)
  (define-key vertico-map "\M-\d" #'vertico-directory-delete-word)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  (with-eval-after-load 'evil
    (define-key vertico-map (kbd "M-h") #'vertico-directory-up)))

(use-package vertico-repeat
  :hook (minibuffer-setup . vertico-repeat-save))

(use-package consult
  :straight t
  :init
  (define-key minibuffer-local-map (kbd "C-r") 'consult-history)
  (global-set-key (kbd "C-s") 'consult-line)
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  :general
  (+map
    ;; Buffers
    "bl"  #'consult-line
    "bb"  #'consult-buffer
    "bmM" #'consult-bookmark
    ;; Files
    "fr"  #'consult-recent-file
    ;; Search
    "ss"  #'consult-ripgrep
    "sM"  #'consult-man
    "st"  #'consult-locate
    ;; Code
    "cx"  #'consult-xref
    "cm"  #'consult-flymake
    ;; Insert
    "iy"  #'consult-yank-pop
    ;; Help
    "hu"  #'consult-theme)
  :config
  (setq-default completion-in-region-function #'consult-completion-in-region))


(provide 'me-completion)
