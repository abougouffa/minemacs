;;; me-ui.el --- UI stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")


(use-package svg-lib
  :straight t
  :custom
  (svg-lib-icons-dir (concat minemacs-cache-dir "svg-lib/icons/")))

(use-package writeroom-mode
  :straight t
  :init
  (+map! "tw" #'writeroom-mode)
  (defcustom +writeroom-text-scale 1.7
    "The text-scaling level for `writeroom-mode'."
    :group 'minemacs
    :type 'float)
  (defcustom +writeroom-enable-mixed-pitch t
    "Enable `mixed-pitch-mode' with `writeroom-mode' for some modes defined in `+writeroom-mixed-pitch-modes'."
    :group 'minemacs
    :type 'boolean)
  (defcustom +writeroom-mixed-pitch-modes
    '(rst-mode markdown-mode org-mode)
    "Enable `mixed-pitch-mode' with `writeroom-mode' for these modes."
    :group 'minemacs
    :type 'boolean)
  :hook (writeroom-mode . +writeroom--enable-text-scaling-mode-h)
  :hook (writeroom-mode . +writeroom--enable-mixed-pitch-mode-maybe-h)
  :hook (writeroom-mode-enable . +writeroom--disable-line-numbers-mode-h)
  :hook (writeroom-mode-disable . +writeroom--restore-line-numbers-mode-h)
  :custom
  (writeroom-width 80)
  (writeroom-mode-line t)
  (writeroom-global-effects nil)
  (writeroom-maximize-window nil)
  (writeroom-fullscreen-effect 'maximized)
  :config
  (defvar-local +writeroom-line-nums-was-active-p nil)
  (defvar-local +writeroom-org-format-latex-scale nil)

  ;; Disable line numbers when in Org mode
  (defun +writeroom--disable-line-numbers-mode-h ()
    (when (and (or (derived-mode-p 'org-mode)
                   (derived-mode-p 'markdown-mode))
               (bound-and-true-p display-line-numbers-mode))
      (setq-local +writeroom-line-nums-was-active-p display-line-numbers-type)
      (display-line-numbers-mode -1)))

  (defun +writeroom--restore-line-numbers-mode-h ()
    (when (and (or (derived-mode-p 'org-mode)
                   (derived-mode-p 'markdown-mode))
               +writeroom-line-nums-was-active-p)
      (display-line-numbers-mode +writeroom-line-nums-was-active-p)))

  (defun +writeroom--enable-mixed-pitch-mode-maybe-h ()
    "Enable `mixed-pitch-mode' when in supported modes."
    (when (and +writeroom-enable-mixed-pitch
               (apply #'derived-mode-p +writeroom-mixed-pitch-modes))
      (require 'mixed-pitch)
      (mixed-pitch-mode (if writeroom-mode 1 -1))))

  (defun +writeroom--enable-text-scaling-mode-h ()
    "Enable text scaling."
    (when (/= +writeroom-text-scale 0.0)
      (text-scale-set (if writeroom-mode +writeroom-text-scale 0.0))
      (visual-fill-column-adjust)))

  (with-eval-after-load 'org
    ;; Increase latex previews scale in Zen mode
    (add-hook
     'writeroom-mode-enable-hook
     (defun +writeroom--scale-up-latex-h ()
       (setq-local
        +writeroom-org-format-latex-scale
        (plist-get org-format-latex-options :scale)
        org-format-latex-options
        (plist-put
         org-format-latex-options
         :scale
         (* ;; The scale from current font
          (/ (float (or (face-attribute 'default :height) 100)) 100.0)
          ;; Proportional upscaling
          (/ +writeroom-text-scale (if (+emacs-features-p 'pgtk) 1.8 1.4)))))))

    (add-hook
     'writeroom-mode-disable-hook
     (defun +writeroom--scale-down-latex-h ()
       (setq-local
        org-format-latex-options
        (plist-put org-format-latex-options
                   :scale (or +writeroom-org-format-latex-scale 1.0)))))))

(use-package mixed-pitch
  :straight t
  :init
  (+map! "tm" #'mixed-pitch-mode)
  :custom
  (mixed-pitch-variable-pitch-cursor 'box)
  :config
  (setq mixed-pitch-fixed-pitch-faces
        (delete-dups
         (append mixed-pitch-fixed-pitch-faces
                 '(org-date
                   org-footnote
                   org-drawer
                   org-special-keyword
                   org-property-value
                   org-column-title
                   org-column
                   org-cite
                   org-cite-key
                   org-ref-cite-face
                   org-tag
                   org-table
                   org-tag-group
                   org-formula
                   org-meta-line
                   org-document-info-keyword
                   org-block
                   org-block-begin-line
                   org-block-end-line
                   org-inline-src-block
                   org-src
                   org-verbatim
                   org-code
                   org-quote
                   org-verse
                   org-latex-and-related
                   org-macro
                   org-link
                   org-sexp-date
                   org-todo
                   org-done
                   font-lock-comment-face
                   font-lock-comment-delimiter-face)))))

(use-package page-break-lines
  :straight t
  :hook ((prog-mode text-mode special-mode) . page-break-lines-mode))

(use-package focus
  :straight t
  :init
  (+map! "tF" #'focus-mode))


(provide 'me-ui)
