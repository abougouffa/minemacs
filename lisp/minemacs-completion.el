;; -*- lexical-binding: t; -*-

(use-package cape
  :straight t
  :config
  (global-set-key (kbd "C-s") 'consult-line)
  (define-key minibuffer-local-map (kbd "C-r") 'consult-history)
  (setq completion-in-region-function #'consult-completion-in-region))

(use-package corfu
  :straight t
  :config
  ;; Setup corfu for popup like completion
  (setq corfu-cycle t ; Allows cycling through candidates
        corfu-auto t  ; Enable auto completion
        corfu-auto-prefix 2 ; Complete with less prefix keys
        corfu-auto-delay 0.0 ; No delay for completion
        corfu-min-width 25
        corfu-scroll-margin 4
        corfu-preselect-first t
        corfu-echo-documentation 0.25) ; Echo docs for current completion option

  (global-corfu-mode 1)

  (add-hook 'corfu-mode-hook #'corfu-doc-mode)
  (define-key corfu-map (kbd "M-p") #'corfu-doc-scroll-down)
  (define-key corfu-map (kbd "M-n") #'corfu-doc-scroll-up)
  (define-key corfu-map (kbd "M-d") #'corfu-doc-toggle))

(use-package kind-icon
  :straight t
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
  (kind-icon-blend-frac 0.08)
  (svg-lib-icons-dir (expand-file-name "svg-lib" minemacs-cache-dir)) ; Change cache dir
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)) ; Enable `kind-icon'

;; Add hook to reset cache so the icon colors match my theme
;; NOTE 2022-02-05: This is a hook which resets the cache whenever I switch
;; the theme using my custom defined command for switching themes. If I don't
;; do this, then the backgound color will remain the same, meaning it will not
;; match the background color corresponding to the current theme. Important
;; since I have a light theme and dark theme I switch between. This has no
;; function unless you use something similar
;; (add-hook 'kb/themes-hooks #'(lambda () (interactive) (kind-icon-reset-cache))))


(use-package corfu-doc
  :after corfu
  :straight t)

(use-package embark
  :straight t
  :config
  (global-set-key [remap describe-bindings] #'embark-bindings)
  (global-set-key (kbd "C-.") 'embark-act)
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :straight t
  :config
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

(use-package marginalia
  :straight t
  :config
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  (marginalia-mode 1))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))


(use-package vertico
  :straight t
  :config
  (add-to-list
   'load-path
   (expand-file-name "straight/build/vertico/extensions"
                     straight-base-dir))

  (setq vertico-cycle t)

  (with-eval-after-load 'evil
    (define-key vertico-map (kbd "C-j") 'vertico-next)
    (define-key vertico-map (kbd "C-k") 'vertico-previous)
    (define-key vertico-map (kbd "M-h") 'vertico-directory-up))

  (require 'vertico-directory)
  (vertico-mode 1))

(provide 'minemacs-completion)
