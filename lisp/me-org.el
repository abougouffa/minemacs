;;; org.el --- Org related stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

(use-package org
  :straight t
  :after minemacs-loaded ;; load Org after finishing Emacs startup
  :preface
  ;; Set to nil so we can detect user changes (in config.el)
  (defvar org-directory nil)
  (defvar org-id-locations-file nil)
  (defvar org-attach-id-dir nil)
  (defvar org-babel-python-command nil)
  (setq org-persist-directory (expand-file-name "org/persist/" minemacs-cache-dir)
        org-publish-timestamp-directory (expand-file-name "org/timestamps/" minemacs-cache-dir)
        org-preview-latex-image-directory (expand-file-name "org/latex/" minemacs-cache-dir)
        org-list-allow-alphabetical t)
  (let ((dir (expand-file-name "org/" minemacs-cache-dir)))
    (unless (file-directory-p dir)
      (mkdir dir t)))
  :config
  (setq org-use-property-inheritance t ; it's convenient to have properties inherited
        org-log-done 'time             ; having the time an item is done sounds convenient
        org-list-allow-alphabetical t  ; have a. A. a) A) list bullets
        org-export-in-background nil   ; run export processes in external emacs process
        org-export-async-debug t
        org-tags-column 0
        org-catch-invisible-edits 'smart ;; try not to accidently do weird stuff in invisible regions
        org-export-with-sub-superscripts t ;; don't treat lone _ / ^ as sub/superscripts, require _{} / ^{}
        org-pretty-entities-include-sub-superscripts nil
        org-fontify-quote-and-verse-blocks t
        org-inline-src-prettify-results '("⟨" . "⟩")
        doom-themes-org-fontify-special-tags nil
        org-auto-align-tags nil
        org-special-ctrl-a/e t
        org-startup-indented t ;; Enable 'org-indent-mode' by default, override with '+#startup: noindent' for big files
        org-insert-heading-respect-content t
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-ellipsis " ↩"
        org-hide-leading-stars t
        org-babel-default-header-args
        '((:session  . "none")
          (:results  . "replace")
          (:exports  . "code")
          (:cache    . "no")
          (:noweb    . "no")
          (:hlines   . "no")
          (:tangle   . "no")
          (:comments . "link")))

  (let ((size 1.3))
    (dolist (face '(org-level-1
                    org-level-2
                    org-level-3
                    org-level-4
                    org-level-5))
      (set-face-attribute face nil :weight 'semi-bold :height size)
      (let ((new-size (* size 0.9)))
        (setq size (if (> new-size 1.0) new-size 1.0)))))

  (defvar +org-responsive-image-percentage 0.4)
  (defvar +org-responsive-image-width-limits '(400 . 700)) ;; '(min . max)

  (defun +org--responsive-image-h ()
    (when (derived-mode-p 'org-mode)
      (setq-local
       org-image-actual-width
       (max (car +org-responsive-image-width-limits)
            (min (cdr +org-responsive-image-width-limits)
                 (truncate (* (window-pixel-width)
                              +org-responsive-image-percentage)))))))

  (add-hook 'window-configuration-change-hook
            #'+org--responsive-image-h))


(use-package org-contrib
  :straight t
  :after org)


(use-package org-appear
  :straight t
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil)
  ;; for proper first-time setup, `org-appear--set-elements'
  ;; needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-elements))


(use-package org-modern
  :straight t
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star '("◉" "○" "◈" "◇" "✳" "◆" "✸" "▶")
        org-modern-table-vertical 5
        org-modern-table-horizontal 2
        org-modern-list '((43 . "➤") (45 . "–") (42 . "•"))
        org-modern-footnote (cons nil (cadr org-script-display))
        org-modern-priority t
        org-modern-block t
        org-modern-block-fringe nil
        org-modern-horizontal-rule t
        org-modern-keyword
        '((t                     . t)
          ("title"               . "𝙏")
          ("subtitle"            . "𝙩")
          ("author"              . "𝘼")
          ("email"               . "@")
          ("date"                . "𝘿")
          ("lastmod"             . "✎")
          ("property"            . "☸")
          ("options"             . "⌥")
          ("startup"             . "⏻")
          ("macro"               . "𝓜")
          ("bind"                . #("" 0 1 (display (raise -0.1))))
          ("bibliography"        . "")
          ("print_bibliography"  . #("" 0 1 (display (raise -0.1))))
          ("cite_export"         . "⮭")
          ("print_glossary"      . #("ᴬᶻ" 0 1 (display (raise -0.1))))
          ("glossary_sources"    . #("" 0 1 (display (raise -0.14))))
          ("export_file_name"    . "⇒")
          ("include"             . "⇤")
          ("setupfile"           . "⇐")
          ("html_head"           . "🅷")
          ("html"                . "🅗")
          ("latex_class"         . "🄻")
          ("latex_class_options" . #("🄻" 1 2 (display (raise -0.14))))
          ("latex_header"        . "🅻")
          ("latex_header_extra"  . "🅻⁺")
          ("latex"               . "🅛")
          ("beamer_theme"        . "🄱")
          ("beamer_color_theme"  . #("🄱" 1 2 (display (raise -0.12))))
          ("beamer_font_theme"   . "🄱𝐀")
          ("beamer_header"       . "🅱")
          ("beamer"              . "🅑")
          ("attr_latex"          . "🄛")
          ("attr_html"           . "🄗")
          ("attr_org"            . "⒪")
          ("name"                . "⁍")
          ("header"              . "›")
          ("caption"             . "☰")
          ("RESULTS"             . "🠶")
          ("language"            . "𝙇")
          ("hugo_base_dir"       . "𝐇")
          ("latex_compiler"      . "⟾")
          ("results"             . "🠶")
          ("filetags"            . "#")
          ("created"             . "⏱")
          ("export_select_tags"  . "✔")
          ("export_exclude_tags" . "❌"))))


(provide 'me-org)

;;; org.el ends here
