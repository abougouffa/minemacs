;;; completion.el --- Completion packages -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

(use-package cape
  :straight t
  :after minemacs-loaded
  :config
  (add-to-list 'completion-at-point-functions #'cape-file) ;; complete file names
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))


(use-package corfu
  :straight t
  :after minemacs-loaded
  :config
  ;; Setup corfu for popup like completion
  (setq corfu-cycle t ; Allows cycling through candidates
        corfu-auto t ; Enable auto completion
        corfu-auto-prefix 2 ; Complete with less prefix keys
        corfu-auto-delay 0.0 ; No delay for completion
        corfu-min-width 25
        corfu-count 10
        corfu-scroll-margin 4
        corfu-preselect-first t
        corfu-echo-documentation 0.25) ; Echo docs for current completion option
  (global-corfu-mode 1))


(use-package corfu-doc
  :straight t
  :hook (corfu-mode . corfu-doc-mode)
  :config
  (setq corfu-doc-auto t
        corfu-doc-delay 0.1
        corfu-doc-max-height 15)
  (define-key corfu-map (kbd "M-p") #'corfu-doc-scroll-down)
  (define-key corfu-map (kbd "M-n") #'corfu-doc-scroll-up)
  (define-key corfu-map (kbd "M-d") #'corfu-doc-toggle))


(use-package svg-lib
  :straight t
  :defer t
  :custom
  (svg-lib-icons-dir (expand-file-name "svg-lib" minemacs-cache-dir))) ; Change cache dir


(use-package kind-icon
  :straight t
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
  (kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)) ; Enable `kind-icon'


(use-package embark
  :straight t
  :after minemacs-loaded
  :config
  (global-set-key [remap describe-bindings] #'embark-bindings)
  (global-set-key (kbd "C-.") 'embark-act)
  (setq prefix-help-command #'embark-prefix-help-command))


(use-package embark-consult
  :straight t
  :after minemacs-loaded)


(use-package all-the-icons-completion
  :straight t
  :after minemacs-loaded)


(use-package marginalia
  :straight t
  :after minemacs-loaded
  :config
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))

  ;; Icons integration
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)
  (marginalia-mode 1))


(use-package orderless
  :straight t
  :after minemacs-loaded
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))


(use-package vertico
  :straight t
  :after minemacs-loaded
  :config
  (add-to-list
   'load-path
   (expand-file-name
    (format "straight/%s/vertico/extensions" straight-build-dir)
    straight-base-dir))

  (setq vertico-cycle t
        vertico-count 12)

  (with-eval-after-load 'evil
    (define-key vertico-map (kbd "C-j") 'vertico-next)
    (define-key vertico-map (kbd "C-k") 'vertico-previous)
    (define-key vertico-map (kbd "M-h") 'vertico-directory-up))

  (require 'vertico-directory)
  (vertico-mode 1))


(use-package consult
  :straight t
  :after minemacs-loaded
  :general
  (me-global-def
    "bl" '(consult-line :which-key "Consult line")
    "ss" '(consult-ripgrep :which-key "ripgrep")
    "ss" '(consult-find :which-key "ripgrep")
    "iy" '(consult-yank-pop :which-key "From clipboard")
    "bb" '(consult-buffer :which-key "Switch to buffer")
    "fr" '(consult-recent-file :which-key "Recent files"))
  :config
  (global-set-key (kbd "C-s") 'consult-line)
  (define-key minibuffer-local-map (kbd "C-r") 'consult-history)
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)
  (setq completion-in-region-function #'consult-completion-in-region))


(provide 'me-completion)
