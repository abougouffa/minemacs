;;; me-org.el --- Org related stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(use-package org
  :straight (:type built-in)
  :after minemacs-loaded
  :preface
  ;; Set to nil so we can detect user changes (in config.el)
  (setq org-directory nil)
  :custom
  (org-tags-column 0)
  (org-auto-align-tags nil)
  (org-startup-indented nil)
  (org-cycle-hide-block-startup t)
  (org-return-follows-link t) ; RET follows link (a key bind has to be defined for Evil, see below)
  (org-fold-catch-invisible-edits 'smart) ; try not to accidently do weird stuff in invisible regions
  (org-fold-core-style 'overlays) ; Fix `evil' search problem (to be used with `evil-search')
  (org-fontify-quote-and-verse-blocks t)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)
  (org-hide-emphasis-markers t)
  (org-use-property-inheritance t) ; it's convenient to have properties inherited
  (org-ellipsis " ↩")
  (org-log-done 'time) ; having the time an item is done sounds convenient
  (org-list-allow-alphabetical t) ; have a. A. a) A) list bullets
  (org-export-in-background t) ; run export processes in external emacs process
  (org-export-async-init-file (expand-file-name (concat minemacs-modules-dir "extras/me-org-export-async-init.el")))
  (org-export-with-smart-quotes t) ; convert "this" to « this »
  (org-export-with-sub-superscripts '{}) ; Only explicit _{} ^{} are interpreted as sub/superscripts
  (org-highlight-latex-and-related '(native script entities))
  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts t)
  (org-use-sub-superscripts '{}) ; Do the same when rendering the Org buffer
  (org-edit-src-content-indentation 0) ; do not indent the content of src blocks
  (org-edit-src-turn-on-auto-save t) ; auto-save org-edit-src
  (org-edit-src-auto-save-idle-delay auto-save-timeout) ; use the defaults
  :config
  (+map-local :keymaps 'org-mode-map
    "l"  '(nil :wk "link")
    "ll" #'org-insert-link
    "e"  #'org-export-dispatch
    "s"  #'org-edit-src-code)
  (+map-local :keymaps 'org-src-mode-map
    "s" #'org-edit-src-save
    "q" #'org-edit-src-abort
    "e" #'org-edit-src-exit)
  (+map-key
    :keymaps 'org-mode-map
    :states 'normal
    "RET" #'org-open-at-point)

  ;; Tectonic can be interesting, however, it don't work right now
  ;; with some of my documents (natbib + sagej...)
  (when (and (executable-find "tectonic") nil)
    (setq org-latex-pdf-process
          '("tectonic -Z shell-escape -Z continue-on-errors --outdir=%o %f"))) ;; --synctex

  (setq org-export-async-debug minemacs-debug) ;; Can be useful!

  ;; Dynamically change font size for Org heading levels, starting from
  ;; `+org-level-base-size', and shrinking by a factor of 0.9 at each level.
  (defvar +org-level-base-size 1.3)

  (dotimes (level 8)
    (let ((size (max 1.0 (* +org-level-base-size (expt 0.9 level)))))
      (set-face-attribute
       (intern (format "org-level-%d" (1+ level))) nil
       :weight 'bold
       :height size)))

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
     (julia . t)
     (sqlite . t)
     (octave . t)
     (maxima . t)
     (eshell . t)
     (scheme . t)
     (python . t)
     (fortran . t)
     (gnuplot . t)
     (plantuml . t)
     (makefile . t)))

  (with-eval-after-load 'org-src
    (setq org-src-lang-modes
          (append
           '(("dot" . graphviz-dot))
           (delete (assoc "dot" org-src-lang-modes #'equal) org-src-lang-modes))))

  (with-eval-after-load 'plantuml-mode
    (setq org-plantuml-jar-path plantuml-jar-path
          org-plantuml-exec-mode plantuml-default-exec-mode
          org-plantuml-executable-path plantuml-executable-path)))

(use-package me-org-extras
  :after org
  :demand t
  :config
  (+org-extras-outline-path-setup)
  (+org-extras-pretty-latex-fragments-setup)
  (+org-extras-latex-classes-setup)
  (+org-extras-responsive-images-setup)
  (+org-extras-equation-numbering-setup)
  (+org-extras-multifiles-document-setup)
  (+org-extras-lower-case-keywords-and-properties-setup))

(use-package org-contrib
  :straight t
  :after org
  :demand t)

(use-package engrave-faces
  :straight t
  :after org
  :demand t
  :custom
  (org-latex-src-block-backend 'engraved))

;; Org export
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

(use-package ox-hugo
  :straight t
  :after org
  :demand t)

(use-package ox-extra
  :after org
  :demand t
  :config
  (ox-extras-activate '(latex-header-blocks ignore-headlines)))

;; Other Org features
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
  :hook (org-agenda-finalize . org-modern-agenda)
  :custom-face
  ;; Force monospaced font for tags
  (org-modern-tag ((t (:inherit org-verbatim :weight regular :foreground "black" :background "LightGray" :box "black"))))
  :custom
  (org-modern-star '("◉" "○" "◈" "◇" "✳" "◆" "✸" "▶"))
  (org-modern-table-vertical 5)
  (org-modern-table-horizontal 2)
  (org-modern-list '((?+ . "➤") (?- . "–") (?* . "•")))
  (org-modern-block-fringe nil)
  (org-modern-todo-faces
   ;; Tweak colors, and force it to be monospaced, useful when using
   ;; mixed-pitch-mode.
   '(("IDEA" . (:inherit org-verbatim :weight semi-bold
                :foreground "white" :background "goldenrod"))
     ("NEXT" . (:inherit org-verbatim :weight semi-bold
                :foreground "white" :background "IndianRed1"))
     ("STRT" . (:inherit org-verbatim :weight semi-bold
                :foreground "white" :background "OrangeRed"))
     ("WAIT" . (:inherit org-verbatim :weight semi-bold
                :foreground "white" :background "coral"))
     ("KILL" . (:inherit org-verbatim :weight semi-bold
                :foreground "white" :background "DarkGreen"))
     ("PROJ" . (:inherit org-verbatim :weight semi-bold
                :foreground "white" :background "LimeGreen"))
     ("HOLD" . (:inherit org-verbatim :weight semi-bold
                :foreground "white" :background "orange"))
     ("DONE" . (:inherit org-verbatim :weight semi-bold
                :foreground "black" :background "LightGray")))))

(use-package org-agenda
  :straight (:type built-in)
  :custom
  (org-agenda-tags-column 0)
  (org-agenda-block-separator ?─)
  (org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────"))

;; For latex fragments
(use-package org-fragtog
  :straight t
  :hook (org-mode . org-fragtog-mode)
  :custom
  (org-fragtog-preview-delay 0.2))

(use-package org-present
  :straight t
  :init
  (+map "oP" :keymaps 'org-mode-map #'org-present)
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
