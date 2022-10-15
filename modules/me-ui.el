;; -*- lexical-binding: t; -*-


(use-package emojify
  :straight t
  :after minemacs-loaded
  :disabled (>= emacs-major-version 29)
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
  (svg-lib-icons-dir (expand-file-name "svg-lib/" minemacs-cache-dir)))


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
  :general
  (me-map "tw" #'writeroom-mode)
  :init
  (defvar +writeroom-text-scale 1.7
    "The text-scaling level for `writeroom-mode'.")
  :custom
  (writeroom-width 80)
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
     (when (/= +writeroom-text-scale 0.0)
       (text-scale-set (if writeroom-mode +writeroom-text-scale 0.0))
       (visual-fill-column-adjust))))

  (defvar-local +writeroom-line-num-was-activate-p nil)

  ;; Disable line numbers when in Org mode
  (add-hook
   'writeroom-mode-enable-hook
   (defun +writeroom--disable-line-numbers-mode-h ()
     (when (and (or (derived-mode-p 'org-mode)
                    (derived-mode-p 'markdown-mode))
                (bound-and-true-p display-line-numbers-mode))
       (setq-local +writeroom-line-num-was-activate-p display-line-numbers-type)
       (display-line-numbers-mode -1))))

  (add-hook
   'writeroom-mode-disable-hook
   (defun +writeroom--restore-line-numbers-mode-h ()
     (when (and (or (derived-mode-p 'org-mode)
                    (derived-mode-p 'markdown-mode))
                +writeroom-line-num-was-activate-p)
       (display-line-numbers-mode +writeroom-line-num-was-activate-p)))))


(use-package mixed-pitch
  :straight t
  :general
  (me-map "tm" #'mixed-pitch-mode)
  :custom
  (mixed-pitch-variable-pitch-cursor t)
  :config
  (setq mixed-pitch-fixed-pitch-faces
        (delete-dups
         (append mixed-pitch-fixed-pitch-faces
                 '(org-date
                   org-footnote
                   org-drawer
                   org-special-keyword
                   org-property-value
                   org-cite-key
                   org-ref-cite-face
                   org-tag
                   org-tag-group
                   org-block
                   org-inline-src-block
                   org-todo-keyword-todo
                   org-latex-and-related
                   org-macro
                   org-link
                   org-todo-keyword-habt
                   org-todo-keyword-done
                   org-todo-keyword-wait
                   org-todo-keyword-kill
                   org-todo-keyword-outd
                   org-todo
                   org-done
                   font-lock-comment-face)))))


(use-package focus
  :straight t
  :general
  (me-map "tF" #'focus-mode))



(provide 'me-ui)
