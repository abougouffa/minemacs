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
  :after minemacs-loaded
  :custom
  (corfu-auto t) ; Enable auto completion
  (corfu-cycle t) ; Allows cycling through candidates
  (corfu-min-width 25)
  (corfu-auto-delay 0.2)
  (corfu-echo-documentation nil)
  :config
  (with-eval-after-load 'evil
    (define-key corfu-map (kbd "C-j") 'corfu-next)
    (define-key corfu-map (kbd "C-k") 'corfu-previous))

  (defun +corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (corfu-mode 1)))

  (add-hook 'minibuffer-setup-hook #'+corfu-enable-in-minibuffer)
  (global-corfu-mode 1))


(use-package corfu-doc
  :straight t
  :hook (corfu-mode . corfu-doc-mode)
  :custom
  (corfu-doc-auto t)
  (corfu-doc-delay 0.1)
  (corfu-doc-max-height 15)
  :config
  (define-key corfu-map (kbd "M-p") #'corfu-doc-scroll-down)
  (define-key corfu-map (kbd "M-n") #'corfu-doc-scroll-up)
  (define-key corfu-map (kbd "M-d") #'corfu-doc-toggle))


(use-package kind-icon
  :straight t
  :after corfu
  :custom
  (kind-icon-default-style ;; Fix the scaling/height
   '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.8 :scale 1.05))
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil) ; Use midpoint color between foreground and background colors ("blended")?
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)) ; Enable `kind-icon'


(use-package embark
  :straight t
  :after minemacs-loaded
  :config
  (setq prefix-help-command #'embark-prefix-help-command)
  (global-set-key [remap describe-bindings] #'embark-bindings)
  (global-set-key (kbd "C-.") 'embark-act))


(use-package embark-consult
  :straight t
  :after embark consult)


(use-package all-the-icons-completion
  :straight t
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))


(use-package marginalia
  :straight t
  :after minemacs-loaded
  :config
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
  :custom
  (vertico-cycle t)
  (vertico-resize nil)
  (vertico-count 12)
  :init
  (add-to-list
   'load-path
   (expand-file-name
    (format "straight/%s/vertico/extensions" straight-build-dir)
    straight-base-dir))
  :config
  (with-eval-after-load 'evil
    (define-key vertico-map (kbd "C-j") #'vertico-next)
    (define-key vertico-map (kbd "C-k") #'vertico-previous))

  (vertico-mode 1))


(use-package vertico-directory
  :after vertico
  :custom
  (vertico-buffer-display-action
   `(display-buffer-at-bottom
     (window-height . ,(+ 3 vertico-count))))
  :config
  (require 'vertico-directory)

  (define-key vertico-map "\r" #'vertico-directory-enter)
  (define-key vertico-map "\d" #'vertico-directory-delete-char)
  (define-key vertico-map "\M-\d" #'vertico-directory-delete-word)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  (with-eval-after-load 'evil
    (define-key vertico-map (kbd "M-h") #'vertico-directory-up)))


(use-package vertico-repeat
  :after vertico
  :config
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))


(use-package mini-popup
  :straight (:host github :repo "minad/mini-popup")
  :after vertico
  :config
  (defun mini-popup-height-resize ()
    (* (1+ (min vertico--total vertico-count)) (default-line-height)))

  (defun mini-popup-height-fixed ()
    (* (1+ (if vertico--input vertico-count 0)) (default-line-height)))

  (setq mini-popup--height-function #'mini-popup-height-fixed)

  ;; Disable the minibuffer resizing of Vertico (HACK)
  (advice-add #'vertico--resize-window :around
              (lambda (&rest args)
                (unless mini-popup-mode
                  (apply args))))

  ;; Ensure that the popup is updated after refresh (Consult-specific)
  (add-hook 'consult--completion-refresh-hook
            (lambda (&rest _) (mini-popup--setup)) 99))


(use-package consult
  :straight t
  :after minemacs-loaded
  :general
  (+map
    "bl" '(consult-line :wk "Consult line")
    "ss" #'consult-ripgrep
    "sc" #'consult-find
    "iy" '(consult-yank-pop :wk "From clipboard")
    "bb" '(consult-buffer :wk "Switch to buffer")
    "fr" '(consult-recent-file :wk "Recent files"))
  :config
  (global-set-key (kbd "C-s") 'consult-line)
  (define-key minibuffer-local-map (kbd "C-r") 'consult-history)
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)
  (setq-default completion-in-region-function #'consult-completion-in-region))


(provide 'me-completion)
