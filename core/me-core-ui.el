;; me-core-ui.el --- MinEmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(defun +theme--tweaks-h (&rest _args)
  "Use smaller font (75% of the default) for line numbers in graphic mode."
  (when (display-graphic-p)
    (set-face-attribute
     'line-number nil
     :background (face-attribute 'default :background)
     :height (truncate (* 0.75 (face-attribute 'default :height)))
     :weight 'semi-light)
    (set-face-attribute
     'line-number-current-line nil
     :height (truncate (* 0.75 (face-attribute 'default :height)))
     :weight 'bold)))

;; Apply tweaks
(add-hook 'after-init-hook #'+theme--tweaks-h)
(add-hook 'enable-theme-functions #'+theme--tweaks-h)

;; Save enabled theme
(add-hook
 'enable-theme-functions
 (defun +theme--save-enabled-theme-h (theme)
   "Save the enabled theme to `minemacs-theme'.
Useful for keeping track of the enabled theme."
   (setq minemacs-theme theme)))

;; Disable previously enabled custom themes before enabling a new one.
(advice-add
 'load-theme :before
 (defun +theme--disable-previous-themes-a (&rest _)
   "Disable previously enabled themes before enabling the new one."
   (mapc #'disable-theme custom-enabled-themes)))

;; Icons
(use-package all-the-icons
  :straight t
  :defer t
  :config
  ;; Show .m files as matlab/octave files
  (setcdr (assoc "m" all-the-icons-extension-icon-alist)
          (cdr (assoc "matlab" all-the-icons-extension-icon-alist))))

(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-one-light t))

(use-package modus-themes
  :straight t
  :defer t
  :custom
  (modus-themes-hl-line '(accented intense))
  (modus-themes-subtle-line-numbers t)
  (modus-themes-region '(bg-only no-extend accented)) ;;
  (modus-themes-variable-pitch-ui nil)
  (modus-themes-fringes nil)
  (modus-themes-diffs nil)
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-intense-mouseovers t)
  (modus-themes-paren-match '(bold intense))
  (modus-themes-syntax '(green-strings alt-syntax))
  (modus-themes-links '(neutral-underline background))
  (modus-themes-mode-line '(borderless padded))
  (modus-themes-tabs-accented nil) ;; default
  (modus-themes-completions
   '((matches   . (extrabold intense accented))
     (selection . (semibold accented intense))
     (popup     . (accented))))
  (modus-themes-headings '((1 . (rainbow 1.4))
                           (2 . (rainbow 1.3))
                           (3 . (rainbow 1.2))
                           (4 . (rainbow bold 1.1))
                           (t . (rainbow bold))))
  (modus-themes-org-blocks 'tinted-background)
  (modus-themes-org-agenda
   '((header-block . (semibold 1.4))
     (header-date  . (workaholic bold-today 1.2))
     (event        . (accented italic varied))
     (scheduled    . rainbow)
     (habit        . traffic-light)))
  (modus-themes-markup '(intense background))
  (modus-themes-mail-citations 'intense)
  (modus-themes-lang-checkers '(intense))

  :config
  (defun +modus-themes-tweak-packages ()
    (modus-themes-with-colors
     (set-face-attribute 'cursor nil :background (modus-themes-color 'blue))
     (set-face-attribute 'font-lock-type-face nil :foreground (modus-themes-color 'magenta-alt))
     (custom-set-faces
      ;; Tweak `evil-mc-mode'
      `(evil-mc-cursor-default-face ((,class :background ,magenta-intense-bg)))
      ;; Tweak `git-gutter-mode'
      `(git-gutter-fr:added ((,class :foreground ,green-fringe-bg)))
      `(git-gutter-fr:deleted ((,class :foreground ,red-fringe-bg)))
      `(git-gutter-fr:modified ((,class :foreground ,yellow-fringe-bg)))
      ;; Tweak `diff-hl-mode'
      `(diff-hl-insert ((,class :foreground ,green-fringe-bg)))
      `(diff-hl-delete ((,class :foreground ,red-fringe-bg)))
      `(diff-hl-change ((,class :foreground ,yellow-fringe-bg)))
      ;; Tweak `doom-modeline'
      `(doom-modeline-evil-normal-state ((,class :foreground ,green-alt-other)))
      `(doom-modeline-evil-insert-state ((,class :foreground ,red-alt-other)))
      `(doom-modeline-evil-visual-state ((,class :foreground ,magenta-alt)))
      `(doom-modeline-evil-operator-state ((,class :foreground ,blue-alt)))
      `(doom-modeline-evil-motion-state ((,class :foreground ,blue-alt-other)))
      `(doom-modeline-evil-replace-state ((,class :foreground ,yellow-alt)))
      ;; Tweak `solaire-mode'
      `(solaire-default-face ((,class :inherit default :background ,bg-alt :foreground ,fg-dim)))
      `(solaire-line-number-face ((,class :inherit solaire-default-face :foreground ,fg-unfocused)))
      `(solaire-hl-line-face ((,class :background ,bg-active)))
      `(solaire-org-hide-face ((,class :background ,bg-alt :foreground ,bg-alt)))
      ;; Tweak `display-fill-column-indicator-mode'
      `(fill-column-indicator ((,class :height 0.3 :background ,bg-inactive :foreground ,bg-inactive)))
      ;; Tweak `mmm-mode'
      `(mmm-cleanup-submode-face ((,class :background ,yellow-refine-bg)))
      `(mmm-code-submode-face ((,class :background ,bg-active)))
      `(mmm-comment-submode-face ((,class :background ,blue-refine-bg)))
      `(mmm-declaration-submode-face ((,class :background ,cyan-refine-bg)))
      `(mmm-default-submode-face ((,class :background ,bg-alt)))
      `(mmm-init-submode-face ((,class :background ,magenta-refine-bg)))
      `(mmm-output-submode-face ((,class :background ,red-refine-bg)))
      `(mmm-special-submode-face ((,class :background ,green-refine-bg))))))

  (add-hook 'modus-themes-after-load-theme-hook #'+modus-themes-tweak-packages)

  (modus-themes-load-operandi))

;; Modeline
(use-package doom-modeline
  :straight t
  :custom
  (doom-modeline-height 45)
  (doom-modeline-bar-width 8)
  (doom-modeline-time-icon nil)
  (doom-modeline-buffer-encoding 'nondefault)
  :config
  ;; Add padding
  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number modals matches follow buffer-info
      remote-host buffer-position word-count parrot selection-info)
    '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug
      repl lsp minor-modes input-method indent-info buffer-encoding major-mode
      process vcs checker time "   "))

  (doom-modeline-mode 1))


(provide 'me-core-ui)
