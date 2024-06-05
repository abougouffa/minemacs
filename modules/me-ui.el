;;; me-ui.el --- UI stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package nerd-icons
  :straight t
  :hook (minemacs-build-functions . nerd-icons-install-fonts)
  :config
  ;; Show .m files as matlab/octave files (integral icon)
  (setcdr (assoc "m" nerd-icons-extension-icon-alist)
          '(nerd-icons-mdicon "nf-md-math_integral_box" :face nerd-icons-orange))
  (when (and (display-graphic-p) (not (+font-installed-p nerd-icons-font-family)))
    (nerd-icons-install-fonts 'dont-ask)))

(use-package doom-themes
  :straight t
  :config
  (with-eval-after-load 'org
    (doom-themes-org-config)))

(use-package doom-modeline
  :straight t
  :unless (memq 'me-nano minemacs-modules)
  :hook (minemacs-lazy . doom-modeline-mode)
  :custom
  (doom-modeline-bar-width 1)
  (doom-modeline-time-icon nil)
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-total-line-number t)
  (doom-modeline-enable-word-count t)
  (doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode rst-mode latex-mode tex-mode text-mode))
  :custom-face
  ;; Hide the modeline bar
  (doom-modeline-bar ((t (:inherit mode-line :background unspecified))))
  (doom-modeline-bar-inactive ((t (:inherit mode-line :background unspecified)))))

(use-package mlscroll
  :straight (:host github :repo "jdtsmith/mlscroll")
  :hook (minemacs-lazy . mlscroll-mode))

(use-package enlight
  :straight (:host github :repo "ichernyshovvv/enlight")
  :when (>= emacs-major-version 29) ; TEMP+BUG: There is an issue with Emacs 28
  :custom
  (enlight-content
   (enlight-menu
    '(("Org Mode"
       ("Org-Agenda (today)" (org-agenda nil "a") "a"))
      ("Projects"
       ("Switch to project" project-switch-project "p")))))
  :init
  (if minemacs-started-with-extra-args-p
      (enlight-open)
    (setq initial-buffer-choice #'enlight)))

(use-package lacarte
  :straight t
  :bind ([f10] . lacarte-execute-menu-command))

(use-package svg-lib
  :straight t
  :custom
  (svg-lib-icons-dir (concat minemacs-cache-dir "svg-lib/icons/")))

(use-package mixed-pitch
  :straight t
  :init
  (+map! "tm" #'mixed-pitch-mode)
  :custom
  (mixed-pitch-variable-pitch-cursor 'box)
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

(use-package focus
  :straight t)

(use-package olivetti
  :straight t)

(use-package logos
  :straight t
  :custom
  ;; If you want to use outlines instead of page breaks (the ^L):
  (logos-outlines-are-pages t)
  ;; This is the default value for the outlines:
  (logos-outline-regexp-alist `((emacs-lisp-mode . "^;;;+ ")
                                (org-mode . "^\\*+ +")
                                (markdown-mode . "^\\#+ +")))
  ;; These apply when `logos-focus-mode' is enabled.  Their value is buffer-local.
  (logos-hide-cursor nil)
  (logos-hide-mode-line t)
  (logos-hide-header-line t)
  (logos-hide-buffer-boundaries t)
  (logos-hide-fringe t)
  (logos-variable-pitch nil)
  (logos-buffer-read-only nil)
  (logos-scroll-lock nil)
  :init
  (let ((map global-map))
    (define-key map [remap narrow-to-region] #'logos-narrow-dwim)
    (define-key map [remap forward-page] #'logos-forward-page-dwim)
    (define-key map [remap backward-page] #'logos-backward-page-dwim)
    (define-key map (kbd "<f9>") #'logos-focus-mode)))

(use-package nerd-icons-ibuffer
  :straight t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package golden-ratio
  :straight t)


(provide 'me-ui)

;;; me-ui.el ends here
