;; -*- lexical-binding: t; -*-

;; Font
(add-hook
 'emacs-startup-hook
 (lambda ()
   (custom-set-faces
    `(default           ((t (:font "Fira Code 15"))))
    `(fixed-pitch       ((t (:inherit (default)))))
    `(fixed-pitch-serif ((t (:inherit (default)))))
    `(variable-pitch    ((t (:font "Fira Code 15")))))))

(setq-default font-lock-multiline 'undecided)

;; Icons
(use-package all-the-icons
	:defer t
  :straight t)

;; Themes
(use-package doom-themes
  :ensure t
  :straight t
  :config
  (load-theme 'doom-one-light t))

(use-package ef-themes
  :straight t
  :unless t
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

  ;; OR use this to load the theme which also calls `ef-themes-post-load-hook':
  (ef-themes-select 'ef-light))

;; Modeline
(use-package doom-modeline
  :straight t
  :config
  (setq doom-modeline-bar-width 5)
  (doom-modeline-mode 1))
