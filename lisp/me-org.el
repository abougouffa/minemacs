;;; org.el --- Org related stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

(use-package org
  :straight t
  :mode ("\\.org\\'" . org-mode)
  :preface
  ;; Set to nil so we can detect user changes to them later (and fall back on
  ;; defaults otherwise).
  (defvar org-directory nil)
  (defvar org-id-locations-file nil)
  (defvar org-attach-id-dir nil)
  (defvar org-babel-python-command nil)
  (setq org-persist-directory (expand-file-name "org/persist/" minemacs-cache-dir)
        org-publish-timestamp-directory (expand-file-name "org/timestamps/" minemacs-cache-dir)
        org-preview-latex-image-directory (expand-file-name "org/latex/" minemacs-etc-dir)
        org-list-allow-alphabetical t)
  :config
  (message "Org is loaded!"))

(use-package org-roam
  :straight t
  :requires org
  :commands (org-roam
             org-roam-ref-find
             org-roam-node-find
             org-roam-node-open
             org-roam-node-insert
             org-roam-node-random))

;; (use-package websocket
;;   :straight t
;;   :after org-roam-ui)

(use-package org-roam-ui
  :straight t
  :commands (org-roam-ui-open org-roam-ui-sync-mode)
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; From https://org-roam.discourse.group/t/configure-deft-title-stripping-to-hide-org-roam-template-headers/478/10
;; (use-package deft
;;   :after org
;;   :bind ("C-c n d" . deft)
;;   :init
;;   (setq deft-directory org-roam-directory
;;         deft-recursive t
;;         deft-use-filter-string-for-filename t
;;         deft-default-extension "org")
;;   :config
;;   (defun +deft-parse-title (file contents)
;;     "Parse the given FILE and CONTENTS and determine the title.
;;      If `deft-use-filename-as-title' is nil, the title is taken to
;;      be the first non-empty line of the FILE.  Else the base name of the FILE is
;;      used as title."
;;     (let ((begin (string-match "^#\\+[tT][iI][tT][lL][eE]: .*$" contents)))
;;       (if begin
;;           (string-trim (substring contents begin (match-end 0)) "#\\+[tT][iI][tT][lL][eE]: *" "[\n\t ]+")
;;         (deft-base-filename file))))

;;   (advice-add 'deft-parse-title :override #'+deft-parse-title)

;;   (setq deft-strip-summary-regexp
;;         (concat "\\("
;;                 "[\n\t]" ;; blank
;;                 "\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
;;                 "\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n" ;; org-roam ID
;;                 "\\|\\[\\[\\(.*\\]\\)" ;; any link
;;                 "\\)")))

(use-package org-contrib
  :straight t
  :after org)

(use-package org-modern
  :straight t
  :after org
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
