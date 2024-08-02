;;; me-ui.el --- UI stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package nerd-icons
  :straight t
  :hook (minemacs-build-functions . nerd-icons-install-fonts)
  :config
  ;; Show .m files as Matlab/Octave files (integral icon)
  (setcdr (assoc "m" nerd-icons-extension-icon-alist) '(nerd-icons-mdicon "nf-md-math_integral_box" :face nerd-icons-orange)))

(use-package doom-themes
  :straight t
  :config
  (with-eval-after-load 'org
    (doom-themes-org-config))
  ;; Enable blinking modeline on errors (`visible-bell')
  (+with-delayed-1! (doom-themes-visual-bell-config)))

(use-package doom-modeline
  :straight t
  :hook (minemacs-lazy . doom-modeline-mode)
  :custom
  (doom-modeline-time-icon nil)
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-total-line-number t)
  (doom-modeline-enable-word-count t)
  (doom-modeline-continuous-word-count-modes '(markdown-mode markdown-ts-mode gfm-mode org-mode rst-mode latex-mode tex-mode)))

(use-package keycast
  :straight t
  :commands (keycast-doom-modeline-mode)
  :config
  (define-minor-mode keycast-doom-modeline-mode
    "Show keycast in `doom-modeline'."
    :global t
    (if keycast-doom-modeline-mode
        (progn (add-hook 'pre-command-hook 'keycast--update t)
               (add-to-list 'global-mode-string '("" keycast-mode-line " ")))
      (remove-hook 'pre-command-hook 'keycast--update)
      (cl-callf2 delete '("" keycast-mode-line " ") global-mode-string))))

(use-package mlscroll
  :straight t
  :hook (minemacs-lazy . +mlscroll-right-mode)
  :init
  ;; For `doom-modeline'
  (define-minor-mode +mlscroll-right-mode
    "Minor mode for displaying an interactive scrollbar in the mode line."
    :global t
    (if +mlscroll-right-mode
        (progn
          (setq mlscroll-right-align nil)
          (add-to-list 'global-mode-string '("" (:eval (mlscroll-mode-line)) " "))
          (mlscroll-layout)
          (add-hook 'enable-theme-functions #'mlscroll-layout)
          (add-hook 'after-make-frame-functions #'mlscroll--update-size)
          (when mlscroll-shortfun-min-width (mlscroll-shortfun-setup)))
      (cl-callf2 delete '("" (:eval (mlscroll-mode-line)) " ") global-mode-string)
      (remove-hook 'after-make-frame-functions #'mlscroll--update-size))))

(use-package enlight
  :straight (:host github :repo "ichernyshovvv/enlight")
  :when (>= emacs-major-version 29) ; TEMP+BUG: There is an issue with Emacs 28
  :custom
  (enlight-content
   (enlight-menu
    '(("Org Mode"
       ("Org-Agenda (today)" (org-agenda nil "a") "a")
       ("Org directory" (dired org-directory) "o"))
      ("Projects"
       ("Switch to project" project-switch-project "p")))))
  :init
  (if minemacs-started-with-extra-args-p
      (enlight-open)
    (setq initial-buffer-choice #'enlight)))

(use-package lacarte
  :straight t
  :bind ([f10] . lacarte-execute-menu-command))

(use-package mixed-pitch
  :straight t
  :custom
  (mixed-pitch-variable-pitch-cursor nil)
  :config
  (setq
   mixed-pitch-fixed-pitch-faces
   (delete-dups
    (append
     mixed-pitch-fixed-pitch-faces
     '(font-lock-comment-delimiter-face font-lock-comment-face org-block
       org-block-begin-line org-block-end-line org-cite org-cite-key
       org-document-info-keyword org-done org-drawer org-footnote org-formula
       org-inline-src-block org-latex-and-related org-link org-code org-column
       org-column-title org-date org-macro org-meta-line org-property-value
       org-quote org-ref-cite-face org-sexp-date org-special-keyword org-src
       org-table org-tag org-tag-group org-todo org-verbatim org-verse)))))

(use-package page-break-lines
  :straight t
  :hook ((prog-mode text-mode special-mode) . page-break-lines-mode))

(use-package pulsar
  :straight t
  :hook (minemacs-first-file . pulsar-global-mode)
  :hook ((next-error xref-after-return) . pulsar-pulse-line) ; only pulse, don't recenter
  :hook ((consult-after-jump imenu-after-jump xref-after-jump) . pulsar-recenter-center) ; pulse and recenter
  :hook ((consult-after-jump imenu-after-jump xref-after-jump xref-after-return) . pulsar-reveal-entry) ; reveal if hidden
  :custom
  (pulsar-face 'pulsar-red)
  :config
  (cl-callf append pulsar-pulse-functions '(what-cursor-position scroll-up-command scroll-down-command)))

(use-package focus
  :straight t)

(use-package olivetti
  :straight t)

(use-package nerd-icons-ibuffer
  :straight t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons-archive
  :straight (:host github :repo "abougouffa/nerd-icons-archive")
  :after arc-mode ; Should be after `tar-mode' or `arc-mode', but `arc-mode' is enough since it is required by the other
  :init
  (nerd-icons-archive-mode 1))

(use-package virtual-format
  :straight (:host github :repo "abougouffa/virtual-format"))

(use-package casual-isearch
  :straight t
  :bind (:package isearch :map isearch-mode-map ([f2] . casual-isearch-tmenu)))

(use-package casual-dired
  :straight t
  :bind (:package dired :map dired-mode-map ([f2] . casual-dired-tmenu)))

(use-package casual-info
  :straight t
  :bind (:package info :map Info-mode-map ([f2] . casual-info-tmenu)))

(use-package casual-calc
  :straight t
  :bind (:package calc :map calc-mode-map ([f2] . casual-calc-tmenu)))

(use-package casual-ibuffer
  :straight t
  :bind (:package ibuffer :map ibuffer-mode-map ([f2] . casual-ibuffer-tmenu)))

(use-package casual-re-builder
  :straight (:host github :repo "kickingvegas/casual-re-builder")
  :bind (:package re-builder :map reb-mode-map ([f2] . casual-re-builder-tmenu)))

(use-package casual-bookmarks
  :straight (:host github :repo "kickingvegas/casual-bookmarks")
  :bind (:package bookmark :map bookmark-bmenu-mode-map ([f2] . casual-bookmarks-tmenu)))

(use-package casual-avy
  :straight t
  :bind ("M-g a" . casual-avy-tmenu))


(provide 'me-ui)

;;; me-ui.el ends here
