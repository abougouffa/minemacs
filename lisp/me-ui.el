;; -*- lexical-binding: t; -*-


;; Icons
(use-package all-the-icons
  :straight t
  :defer t
  :config
  ;; Show .m files as matlab/octave files
  (setcdr (assoc "m" all-the-icons-extension-icon-alist)
          (cdr (assoc "matlab" all-the-icons-extension-icon-alist))))


(use-package svg-lib
  :straight t
  :defer t
  :custom
  (svg-lib-icons-dir (expand-file-name "svg-lib" minemacs-cache-dir))) ; Change cache dir


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
  (setq doom-modeline-bar-width 5
        doom-modeline-height 37)

  (doom-modeline-def-segment time
    (when (and doom-modeline-time
               (bound-and-true-p display-time-mode)
               (not doom-modeline--limited-width-p))
      (concat
       doom-modeline-spc
       (when doom-modeline-time-icon
         (concat
          (doom-modeline-icon 'faicon "clock-o" "🕘" ""
                              :face 'mode-line
                              :v-adjust -0.05)
          (and (or doom-modeline-icon doom-modeline-unicode-fallback)
               doom-modeline-spc)))
       (propertize display-time-string
                   'face (doom-modeline-face 'doom-modeline-time)))))

  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number modals matches follow buffer-info
          remote-host buffer-position word-count parrot selection-info)
    '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug
                  repl lsp minor-modes input-method indent-info buffer-encoding major-mode
                  process vcs checker time "    "))

  (doom-modeline-mode 1))


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
  (me-global-def
    "ie" '(emojify-insert-emoji :which-key "Emoji"))
  :config
  (setq emojify-emojis-dir (expand-file-name "emojify" minemacs-cache-dir)
        emojify-display-style 'image
        emojify-emoji-set "emojione-v2.2.6"))


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


(provide 'me-ui)
