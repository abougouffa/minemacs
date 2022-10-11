;; -*- lexical-binding: t; -*-


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
  (setq ef-themes-mixed-fonts nil
        ef-themes-variable-pitch-ui nil)

  ;; ;; Disable all other themes to avoid awkward blending:
  ;; (mapc #'disable-theme custom-enabled-themes)

  ;; ;; Load the theme of choice:
  ;; (load-theme 'ef-light :no-confirm)

  ;; OR use this to load the theme which also calls `ef-themes-post-load-hook':
  (ef-themes-select 'ef-light))


(use-package emojify
  :straight t
  :after minemacs-loaded
  :general
  (me-map "ie" '(emojify-insert-emoji :which-key "Emoji"))
  :custom
  (emojify-emoji-set "emojione-v2.2.6")
  (emojify-emojis-dir (expand-file-name "emojify" minemacs-cache-dir))
  (emojify-display-style 'image)
  :config
  (global-emojify-mode 1))


(use-package svg-lib
  :straight t
  :defer t
  :custom
  (svg-lib-icons-dir (expand-file-name "svg-lib" minemacs-cache-dir))) ; Change cache dir


(use-package popwin
  :straight (:host github :repo "emacsorphanage/popwin" :files (:defaults "*"))
  :after minemacs-loaded
  :config
  (defun +popwin-register (pred &rest args)
    (if (listp pred)
        (dolist (p pred)
          (push (cons p args) popwin:special-display-config))
      (push (cons pred args) popwin:special-display-config)))

  (+popwin-register '("*Warnings*" compilation-mode) :height 8 :noselect t)
  (popwin-mode 1))


(use-package writeroom-mode
  :straight t
  :defer t
  :general
  (me-map
    "tz" '(writeroom-mode :which-key "Writeroom mode"))
  :init
  (defvar +writeroom-text-scale 2
    "The text-scaling level for `writeroom-mode'.")
  :custom
  (writeroom-width 0.5)
  (writeroom-mode-line t)
  (writeroom-global-effects nil)
  (writeroom-maximize-window nil)
  (writeroom-fullscreen-effect 'maximized)
  :config
  (require 'mixed-pitch)
  (add-hook
   'writeroom-mode-hook
   (defun +writeroom--enable-mixed-pitch-mode-h ()
     "Enable `mixed-pitch-mode' when in supported modes."
     (when (apply #'derived-mode-p '(adoc-mode rst-mode markdown-mode org-mode))
       (mixed-pitch-mode (if writeroom-mode 1 -1)))))

  (add-hook
   'writeroom-mode-hook
   (defun +writeroom--enable-text-scaling-mode-h ()
     "Enable text scaling."
     (when (/= +writeroom-text-scale 0)
       (text-scale-set (if writeroom-mode +writeroom-text-scale 0))
       (visual-fill-column-adjust))))

  ;; Disable line numbers when in Org mode
  (add-hook
   'writeroom-mode-enable-hook
   (defun +writeroom--disable-line-numbers-mode-h ()
     (when (and (or (derived-mode-p 'org-mode) (derived-mode-p 'markdown-mode))
                (bound-and-true-p display-line-numbers-mode))
       (setq-local +line-num--was-activate-p display-line-numbers-type)
       (display-line-numbers-mode -1))))

  (add-hook
   'writeroom-mode-disable-hook
   (defun +writeroom--restore-line-numbers-mode-h ()
     (when (and (or (derived-mode-p 'org-mode) (derived-mode-p 'markdown-mode))
                (bound-and-true-p +line-num--was-activate-p))
       (display-line-numbers-mode +line-num--was-activate-p)))))


(use-package mixed-pitch
  :straight t
  :defer t
  :general
  (me-map
    "tm" '(mixed-pitch-mode :which-key "Mixed-pitch mode"))
  :config
  (setq mixed-pitch-fixed-pitch-faces
        (append mixed-pitch-fixed-pitch-faces
                '(org-date
                  org-footnote
                  org-special-keyword
                  org-property-value
                  org-ref-cite-face
                  org-tag
                  org-todo-keyword-todo
                  org-todo-keyword-habt
                  org-todo-keyword-done
                  org-todo-keyword-wait
                  org-todo-keyword-kill
                  org-todo-keyword-outd
                  org-todo
                  org-done
                  font-lock-comment-face))))


(use-package focus
  :straight t
  :commands focus-mode
  :general
  (me-map
    "tf" '(focus-mode :which-key "Focus mode")))


(provide 'me-ui)
