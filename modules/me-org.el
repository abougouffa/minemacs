;;; me-org.el --- Org related stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

(use-package org
  :straight (:host sourcehut :repo "bzg/org-mode")
  :after minemacs-loaded ;; load Org after finishing Emacs startup
  :preface
  ;; Set to nil so we can detect user changes (in config.el)
  (defvar org-directory nil)
  ;; Create the Org cache directory if it doesn't exist
  (let ((cache-dir (concat minemacs-cache-dir "org/")))
    (unless (file-directory-p cache-dir)
      (mkdir cache-dir t)))
  :custom
  (org-tags-column 0)
  (org-id-locations-file (concat minemacs-cache-dir "org/org-id-locations"))
  (org-persist-directory (concat minemacs-cache-dir "org/persist/"))
  (org-publish-timestamp-directory (concat minemacs-cache-dir "org/timestamps/"))
  (org-preview-latex-image-directory (concat minemacs-cache-dir "org/latex/"))
  (org-auto-align-tags nil)
  (org-return-follows-link t) ; RET follows link (a key bind has to be defined for Evil, see below)
  (org-fold-catch-invisible-edits 'smart) ; try not to accidently do weird stuff in invisible regions
  (org-fontify-quote-and-verse-blocks t)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)
  (org-hide-emphasis-markers t)
  (org-use-property-inheritance t) ; it's convenient to have properties inherited
  (org-ellipsis " ↩")
  (org-log-done 'time) ; having the time an item is done sounds convenient
  (org-list-allow-alphabetical t) ; have a. A. a) A) list bullets
  (org-export-in-background t) ; run export processes in external emacs process
  (org-export-async-init-file (concat minemacs-modules-dir "extras/me-org-export-async-init.el"))
  (org-export-with-smart-quotes t) ; convert "this" to « this »
  (org-export-with-sub-superscripts '{}) ; Only explicit _{} ^{} are interpreted as sub/superscripts
  (org-highlight-latex-and-related '(native script entities))
  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts t)
  (org-use-sub-superscripts '{}) ; Do the same when rendering the Org buffer
  :config
  (+map-local :keymaps 'org-mode-map
    "l"  '(nil :wk "link")
    "ll" '(org-insert-link :wk "Insert link")
    "e"  '(org-export-dispatch :wk "Export dispatch"))

  (+map-key
    :keymaps 'org-mode-map
    :states 'normal
    "RET" #'org-open-at-point)

  ;; Tectonic can be interesting, however, it don't work right now
  ;; with some of my documents (natbib + sagej...)
  (when (and (executable-find "tectonic") nil)
    (setq org-latex-pdf-process
          '("tectonic -Z shell-escape -Z continue-on-errors --outdir=%o %f"))) ;; --synctex

  (setq org-export-async-debug t) ;; Can be useful!

  (let ((size 1.3))
    (dolist (face '(org-level-1 org-level-2 org-level-3 org-level-4 org-level-5))
      (set-face-attribute face nil :weight 'semi-bold :height size)
      (setq size (max (* size 0.9) 1.0))))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (R . t)
     (js . t)
     (dot . t)
     (awk . t)
     (sed . t)
     (sql . t)
     (org . t)
     (shell . t)
     (ditaa . t)
     (latex . t)
     (eshell . t)
     (julia . t)
     (sqlite . t)
     (octave . t)
     (maxima . t)
     (scheme . t)
     (python . t)
     (gnuplot . t)
     (plantuml . t)
     (makefile . t)))

  (setq org-src-lang-modes
        '(("C" . c)
          ("C++" . c++)
          ("asymptote" . asy)
          ("bash" . sh)
          ("beamer" . latex)
          ("calc" . fundamental)
          ("cpp" . c++)
          ("ditaa" . artist)
          ("desktop" . conf-desktop)
          ("dot" . graphviz-dot) ;; changed
          ("elisp" . emacs-lisp)
          ("ocaml" . tuareg)
          ("screen" . shell-script)
          ("shell" . sh)
          ("sqlite" . sql)
          ("toml" . conf-toml)))

  (with-eval-after-load 'plantuml-mode
    (setq org-plantuml-jar-path plantuml-jar-path)))


(use-package org-contrib
  :straight (:host sourcehut :repo "bzg/org-contrib")
  :after org)


(use-package ox-latex
  :after org
  :custom
  (org-latex-prefer-user-labels t)
  ;; Default `minted` options, can be overwritten in file/dir locals
  (org-latex-minted-options
   '(("frame"         "lines")
     ("fontsize"      "\\footnotesize")
     ("tabsize"       "2")
     ("breaklines"    "true")
     ("breakanywhere" "true") ;; break anywhere, no just on spaces
     ("style"         "default")
     ("bgcolor"       "GhostWhite")
     ("linenos"       "true")))
  :config
  ;; Map some org-mode blocks' languages to lexers supported by minted
  ;; you can see supported lexers by running this command in a terminal:
  ;; 'pygmentize -L lexers'
  (dolist (pair '((ipython    "python")
                  (jupyter    "python")
                  (scheme     "scheme")
                  (lisp-data  "lisp")
                  (conf-unix  "unixconfig")
                  (conf-space "unixconfig")
                  (authinfo   "unixconfig")
                  (gdb-script "unixconfig")
                  (conf-toml  "yaml")
                  (conf       "ini")
                  (gitconfig  "ini")
                  (systemd    "ini")))
    (unless (member pair org-latex-minted-langs)
      (add-to-list 'org-latex-minted-langs pair))))


(use-package ob-tangle
  :after org)


(use-package ox-extra
  :after org
  :config
  (ox-extras-activate '(latex-header-blocks ignore-headlines)))


(use-package me-org-extras
  :after org
  :config
  (+org-extras-outline-path-setup)
  (+org-extras-pretty-latex-fragments-setup)
  (+org-extras-latex-classes-setup)
  (+org-extras-responsive-images-setup)
  (+org-extras-equation-numbering-setup)
  (+org-extras-multifiles-document-setup))


(use-package org-appear
  :straight t
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-inside-latex t)
  (org-appear-autokeywords t)
  (org-appear-autoentities t)
  (org-appear-autoemphasis t)
  (org-appear-autosubmarkers t)
  (org-appear-autolinks 'just-brackets)
  :config
  ;; for proper first-time setup, `org-appear--set-elements'
  ;; needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-elements))


(use-package org-modern
  :straight t
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-star '("◉" "○" "◈" "◇" "✳" "◆" "✸" "▶"))
  (org-modern-table-vertical 5)
  (org-modern-table-horizontal 2)
  (org-modern-list '((43 . "➤") (45 . "–") (42 . "•")))
  (org-modern-block-fringe nil))


;; for latex fragments
(use-package org-fragtog
  :straight t
  :hook (org-mode . org-fragtog-mode)
  :custom
  (org-fragtog-preview-delay 0.1))


(use-package org-present
  :straight t
  :general
  (+map "oP" #'org-present)
  :config
  (setq org-present-text-scale 2.5)

  (defvar-local +org-present--vcm-params
      '(:enabled nil
        :width nil
        :center-text nil)
    "Variable to hold `visual-fill-column-mode' parameters")

  (add-hook
   'org-present-mode-hook
   (defun +org-present--on-h ()
     (setq-local
      face-remapping-alist
      '((default (:height 1.5) variable-pitch)
        (header-line (:height 2.0) variable-pitch)
        (org-document-title (:height 2.0) org-document-title)
        (org-code (:height 1.55) org-code)
        (org-verbatim (:height 1.55) org-verbatim)
        (org-block (:height 1.25) org-block)
        (org-block-begin-line (:height 0.7) org-block)))
     ;; (org-present-big)
     (org-display-inline-images)
     (org-present-hide-cursor)
     (org-present-read-only)
     (when (bound-and-true-p visual-fill-column-mode)
       (+plist-push! +org-present--vcm-params
                     :enabled visual-fill-column-mode
                     :width visual-fill-column-width
                     :center-text visual-fill-column-center-text))
     (setq-local visual-fill-column-width 120
                 visual-fill-column-center-text t)
     (visual-fill-column-mode 1)))

  (add-hook
   'org-present-mode-quit-hook
   (defun +org-present--off-h ()
     (setq-local
      face-remapping-alist
      '((default default default)))
     ;; (org-present-small)
     (org-remove-inline-images)
     (org-present-show-cursor)
     (org-present-read-write)
     (visual-fill-column-mode -1)
     (unless (plist-get +org-present--vcm-params :enabled)
       (setq-local visual-fill-column-width (plist-get +org-present--vcm-params :width)
                   visual-fill-column-center-text (plist-get +org-present--vcm-params :center-text))
       (visual-fill-column-mode 1)))))


(provide 'me-org)

;;; me-org.el ends here
