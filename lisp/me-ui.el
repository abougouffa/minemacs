;; -*- lexical-binding: t; -*-


;; Icons
(use-package all-the-icons
  :straight t
  :defer t
  :config
  ;; Show .m files as matlab/octave files
  (setcdr (assoc "m" all-the-icons-extension-icon-alist)
          (cdr (assoc "matlab" all-the-icons-extension-icon-alist))))

;; Themes
(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-one-light t)
  (with-eval-after-load 'org
    (require 'doom-themes-ext-org)))

;; Modeline
(use-package doom-modeline
  :straight t
  :config
  (setq doom-modeline-bar-width 5)
  (doom-modeline-mode 1))

(use-package focus
  :straight t
  :general
  (me-global-def
    "tf" '(focus-mode :which-key "Focus mode"))
  :commands focus-mode)

;;; Disabled, WIP...

(use-package ef-themes
  :straight t
  :disabled t
  ;; If you like two specific themes and want to switch between them, you
  ;; can specify them in `ef-themes-to-toggle' and then invoke the command
  ;; `ef-themes-toggle'.  All the themes are included in the variable
  ;; `ef-themes-collection'.
  :config
  (setq ef-themes-to-toggle '(ef-light ef-day))

    ;; Make customisations that affect Emacs faces BEFORE loading a theme
    ;; (any change needs a theme re-load to take effect).

  (setq ef-themes-headings ; read the manual's entry or the doc string
        '((0 . (variable-pitch light 1.9))
          (1 . (variable-pitch light 1.8))
          (2 . (variable-pitch regular 1.7))
          (3 . (variable-pitch regular 1.6))
          (4 . (variable-pitch regular 1.5))
          (5 . (variable-pitch 1.4)) ; absence of weight means `bold'
          (6 . (variable-pitch 1.3))
          (7 . (variable-pitch 1.2))
          (t . (variable-pitch 1.1))))

    ;; They are nil by default...
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t)

    ;; ;; Disable all other themes to avoid awkward blending:
    ;; (mapc #'disable-theme custom-enabled-themes)

    ;; ;; Load the theme of choice:
    ;; (load-theme 'ef-light :no-confirm)

    ;; OR use this to load the theme which also calls `ef-themes-post-load-hook':
  (ef-themes-select 'ef-light))

(use-package lambda-themes
  :disabled t
  :straight (:type git :host github :repo "lambda-emacs/lambda-themes") 
  :init
  (setq lambda-themes-set-italic-comments t
        lambda-themes-set-italic-keywords t
        lambda-themes-set-variable-pitch t
        lambda-themes-set-evil-cursors t)
  :config
  ;; load preferred theme
  (load-theme 'lambda-light t))

(use-package lambda-line
  :disabled t
  :straight (:type git :host github :repo "lambda-emacs/lambda-line") 
  :custom
  (lambda-line-icon-time t) ;; requires all-the-icons
  (lambda-line-position 'top) ;; Set position of status-line 
  (lambda-line-abbrev t) ;; abbreviate major modes
  (lambda-line-hspace "  ")  ;; add some cushion
  (lambda-line-prefix t) ;; use a prefix symbol
  (lambda-line-prefix-padding nil) ;; no extra space for prefix 
  (lambda-line-status-invert nil)  ;; no invert colors
  (lambda-line-gui-mod-symbol " ⬤") 
  (lambda-line-gui-ro-symbol  " ⨂") ;; symbols
  (lambda-line-gui-rw-symbol  " ◯") 
  (lambda-line-space-top +.50)  ;; padding on top and bottom of line
  (lambda-line-space-bottom -.50)
  (lambda-line-symbol-position 0.1) ;; adjust the vertical placement of symbol
  :config
  ;; activate lambda-line 
  (lambda-line-mode) 
  ;; set divider line in footer
  (when (eq lambda-line-position 'top)
    (setq-default mode-line-format (list "%_"))
    (setq mode-line-format (list "%_"))))


(use-package emojify
  :straight t
  :defer t
  :config
  (setq emojify-emojis-dir (expand-file-name "emojify" minemacs-cache-dir)
        emojify-display-style 'image
        emojify-emoji-set "emojione-v2.2.6"))


(use-package popwin
  :straight t
  :config
  (popwin-mode 1))


(provide 'me-ui)
