;;; me-org.el --- Org related stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

(use-package org
  :straight t
  :after minemacs-loaded ;; load Org after finishing Emacs startup
  :general
  (me-local-def :keymaps 'org-mode-map
    "l"  '(nil :which-key "link")
    "ll" '(org-insert-link :which-key "Insert link")
    "e" '(org-export-dispatch :which-key "Export dispatch"))

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
  (require 'ox-latex nil :noerror)
  (require 'ob-tangle nil :noerror)

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
    (dolist (face '(org-level-1 org-level-2 org-level-3 org-level-4 org-level-5))
      (set-face-attribute face nil :weight 'semi-bold :height size)
      (setq size (max (* size 0.9) 1.0))))

  (defvar +org-responsive-image-percentage 0.4)
  (defvar +org-responsive-image-width-limits '(400 . 700)) ;; '(min . max)

  (defun +org--responsive-image-h ()
    (when (derived-mode-p 'org-mode)
      (setq-local
       org-image-actual-width
       (list (max (car +org-responsive-image-width-limits)
                  (min (cdr +org-responsive-image-width-limits)
                       (truncate (* (window-pixel-width)
                                    +org-responsive-image-percentage))))))))

  (add-hook 'window-configuration-change-hook
            #'+org--responsive-image-h)

  (defun +parse-the-fun (str)
    "Parse the LaTeX environment STR.
  Return an AST with newlines counts in each level."
    (let (ast)
      (with-temp-buffer
        (insert str)
        (goto-char (point-min))
        (while (re-search-forward
                (rx "\\"
                    (group (or "\\" "begin" "end" "nonumber"))
                    (zero-or-one "{" (group (zero-or-more not-newline)) "}"))
                nil t)
          (let ((cmd (match-string 1))
                (env (match-string 2)))
            (cond ((string= cmd "begin")
                   (push (list :env (intern env)) ast))
                  ((string= cmd "\\")
                   (let ((curr (pop ast)))
                     (push (plist-put curr :newline (1+ (or (plist-get curr :newline) 0))) ast)))
                  ((string= cmd "nonumber")
                   (let ((curr (pop ast)))
                     (push (plist-put curr :nonumber (1+ (or (plist-get curr :nonumber) 0))) ast)))
                  ((string= cmd "end")
                   (let ((child (pop ast))
                         (parent (pop ast)))
                     (push (plist-put parent :childs (cons child (plist-get parent :childs))) ast)))))))
      (plist-get (car ast) :childs)))

  (defun +scimax-org-renumber-environment (orig-func &rest args)
    "A function to inject numbers in LaTeX fragment previews."
    (let ((results '())
          (counter -1))
      (setq results
            (cl-loop for (begin . env) in
                     (org-element-map (org-element-parse-buffer) 'latex-environment
                       (lambda (env)
                         (cons
                          (org-element-property :begin env)
                          (org-element-property :value env))))
                     collect
                     (cond
                      ((and (string-match "\\\\begin{equation}" env)
                            (not (string-match "\\\\tag{" env)))
                       (cl-incf counter)
                       (cons begin counter))
                      ((string-match "\\\\begin{align}" env)
                       (cl-incf counter)
                       (let ((p (car (+parse-the-fun env))))
                         ;; Parse the `env', count new lines in the align env as equations, unless
                         (cl-incf counter (- (or (plist-get p :newline) 0)
                                             (or (plist-get p :nonumber) 0))))
                       (cons begin counter))
                      (t
                       (cons begin nil)))))
      (when-let ((number (cdr (assoc (point) results))))
        (setf (car args)
              (concat
               (format "\\setcounter{equation}{%s}\n" number)
               (car args)))))
    (apply orig-func args))

  (defun +scimax-toggle-latex-equation-numbering (&optional enable)
    "Toggle whether LaTeX fragments are numbered."
    (interactive)
    (if (or enable (not (get '+scimax-org-renumber-environment 'enabled)))
        (progn
          (advice-add 'org-create-formula-image :around #'+scimax-org-renumber-environment)
          (put '+scimax-org-renumber-environment 'enabled t)
          (message "LaTeX numbering enabled."))
      (advice-remove 'org-create-formula-image #'+scimax-org-renumber-environment)
      (put '+scimax-org-renumber-environment 'enabled nil)
      (message "LaTeX numbering disabled.")))

  (defun +scimax-org-inject-latex-fragment (orig-func &rest args)
    "Advice function to inject latex code before and/or after the equation in a latex fragment.
  You can use this to set \\mathversion{bold} for example to make
  it bolder. The way it works is by defining
  :latex-fragment-pre-body and/or :latex-fragment-post-body in the
  variable `org-format-latex-options'. These strings will then be
  injected before and after the code for the fragment before it is
  made into an image."
    (setf (car args)
          (concat
           (or (plist-get org-format-latex-options :latex-fragment-pre-body) "")
           (car args)
           (or (plist-get org-format-latex-options :latex-fragment-post-body) "")))
    (apply orig-func args))

  (defun +scimax-toggle-inject-latex ()
    "Toggle whether you can insert latex in fragments."
    (interactive)
    (if (not (get '+scimax-org-inject-latex-fragment 'enabled))
        (progn
          (advice-add 'org-create-formula-image :around #'+scimax-org-inject-latex-fragment)
          (put '+scimax-org-inject-latex-fragment 'enabled t)
          (message "Inject latex enabled"))
      (advice-remove 'org-create-formula-image #'+scimax-org-inject-latex-fragment)
      (put '+scimax-org-inject-latex-fragment 'enabled nil)
      (message "Inject latex disabled")))

  ;; Enable renumbering by default
  (+scimax-toggle-latex-equation-numbering t))


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
          ("bind"                . "")
          ("bibliography"        . "")
          ("print_bibliography"  . "")
          ("cite_export"         . "⮭")
          ("print_glossary"      . "ᴬᶻ")
          ("glossary_sources"    . "ᴬᶻ")
          ("export_file_name"    . "⇒")
          ("include"             . "⇤")
          ("setupfile"           . "⇐")
          ("html_head"           . "🅷")
          ("html"                . "🅗")
          ("latex_class"         . "🄻")
          ("latex_class_options" . "🄻ⓞ")
          ("latex_header"        . "🅻")
          ("latex_header_extra"  . "🅻⁺")
          ("latex"               . "🅛")
          ("beamer_theme"        . "🄱")
          ("beamer_color_theme"  . "🄱ⓣ")
          ("beamer_font_theme"   . "🄱ⓕ")
          ("beamer_header"       . "🅱")
          ("beamer"              . "🅑")
          ("attr_latex"          . "🄛")
          ("attr_html"           . "🄗")
          ("attr_org"            . "⒪")
          ("name"                . "⁍")
          ("header"              . "›")
          ("caption"             . "☰")
          ("RESULTS"             . "➤")
          ("language"            . "𝙇")
          ("hugo_base_dir"       . "𝐇")
          ("latex_compiler"      . "⟾")
          ("results"             . "➤")
          ("filetags"            . "#")
          ("created"             . "⏱")
          ("export_select_tags"  . "✔")
          ("export_exclude_tags" . "⛌"))))


;; for latex fragments
(use-package org-fragtog
  :straight t
  :hook (org-mode . org-fragtog-mode))


(provide 'me-org)

;;; me-org.el ends here
