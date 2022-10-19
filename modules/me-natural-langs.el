;; -*- lexical-binding: t; -*-

(defconst ASPELL-P (executable-find "aspell"))

(use-package spell-fu
  :straight t
  :when ASPELL-P
  :general
  (+map "ts" #'spell-fu-mode)
  :hook (text-mode . spell-fu-mode)
  :init
  (setq spell-fu-directory (expand-file-name "spell-fu" minemacs-local-dir))
  (defvar +spell-excluded-faces-alist
    '((markdown-mode
       . (markdown-code-face
          markdown-html-attr-name-face
          markdown-html-attr-value-face
          markdown-html-tag-name-face
          markdown-inline-code-face
          markdown-link-face
          markdown-markup-face
          markdown-plain-url-face
          markdown-reference-face
          markdown-url-face))
      (org-mode
       . (org-block
          org-block-begin-line
          org-block-end-line
          org-cite
          org-cite-key
          org-code
          org-date
          org-footnote
          org-formula
          org-inline-src-block
          org-latex-and-related
          org-link
          org-meta-line
          org-property-value
          org-ref-cite-face
          org-special-keyword
          org-tag
          org-todo
          org-todo-keyword-done
          org-todo-keyword-habt
          org-todo-keyword-kill
          org-todo-keyword-outd
          org-todo-keyword-todo
          org-todo-keyword-wait
          org-verbatim))
      (latex-mode
       . (font-latex-math-face
          font-latex-sedate-face
          font-lock-function-name-face
          font-lock-keyword-face
          font-lock-variable-name-face)))
    "Faces in certain major modes that spell-fu will not spellcheck.")
  :config
  (unless (file-exists-p spell-fu-directory)
    (mkdir spell-fu-directory t))

  (defun +spell-fu-register-dictionary (lang)
    "Add `LANG` to spell-fu multi-dict, with a personal dictionary."
    ;; Add the dictionary
    (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary lang))
    (let ((personal-dict-file (expand-file-name (format "personal-aspell.%s.pws" lang) spell-fu-directory)))
      ;; Create an empty personal dictionary if it doesn't exists
      (unless (file-exists-p personal-dict-file) (write-region "" nil personal-dict-file))
      ;; Add the personal dictionary
      (spell-fu-dictionary-add (spell-fu-get-personal-dictionary (format "%s-personal" lang) personal-dict-file))))

  (add-hook
   'spell-fu-mode-hook
   (defun +spell-fu--init-excluded-faces-h ()
     "Set `spell-fu-faces-exclude' according to `+spell-excluded-faces-alist'."
     (when-let (excluded (cdr (cl-find-if #'derived-mode-p +spell-excluded-faces-alist :key #'car)))
       (setq-local spell-fu-faces-exclude excluded)))))


(use-package me-spell-fu
  :after spell-fu
  :general
  (+map-key "z=" #'+spell-fu-correct))


(use-package go-translate
  :straight (:host github :repo "lorniu/go-translate")
  :commands (gts-do-translate
             +gts-yank-translated-region
             +gts-translate-with)
  :init
  ;; Your languages pairs
  (setq gts-translate-list '(("en" "fr")
                             ("en" "ar")
                             ("fr" "ar")
                             ("fr" "en")))

  (+map-local :keymaps '(org-mode-map markdown-mode-map latex-mode-map text-mode-map)
    "R" '(+gts-yank-translated-region :wk "Yank translated region")
    "G" '(nil "go-translate")
    "Gb" `(,(+cmdfy! (+gts-translate-with 'bing)) :wk "Bing")
    "Gd" `(,(+cmdfy! (+gts-translate-with 'deepl)) :wk "DeepL")
    "Gg" `(,(+cmdfy! (+gts-translate-with))) :wk "Google"
    "GR" '(+gts-yank-translated-region :wk "Yank translated region")
    "Gt" '(gts-do-translate :wk "gts-do-translate"))

  :config
  ;; Config the default translator, which will be used by the command `gts-do-translate'
  (setq gts-default-translator
        (gts-translator
         ;; Used to pick source text, from, to. choose one.
         :picker (gts-prompt-picker)
         ;; One or more engines, provide a parser to give different output.
         :engines (gts-google-engine :parser (gts-google-summary-parser))
         ;; Render, only one, used to consumer the output result.
         :render (gts-buffer-render)))

  ;; Custom texter which remove newlines in the same paragraph
  (defclass +gts-translate-paragraph (gts-texter) ())

  (cl-defmethod gts-text ((_ +gts-translate-paragraph))
    (when (use-region-p)
      (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
        (with-temp-buffer
          (insert text)
          (goto-char (point-min))
          (let ((case-fold-search nil))
            (while (re-search-forward "\n[^\n]" nil t)
              (replace-region-contents
               (- (point) 2) (- (point) 1)
               (lambda (&optional a b) " ")))
            (buffer-string))))))

  ;; Custom picker to use the paragraph texter
  (defclass +gts-paragraph-picker (gts-picker)
    ((texter :initarg :texter :initform (+gts-translate-paragraph))))

  (cl-defmethod gts-pick ((o +gts-paragraph-picker))
    (let ((text (gts-text (oref o texter))))
      (when (or (null text) (zerop (length text)))
        (user-error "Make sure there is any word at point, or selection exists"))
      (let ((path (gts-path o text)))
        (setq gts-picker-current-path path)
        (cl-values text path))))

  (defun +gts-yank-translated-region ()
    (interactive)
    (gts-translate
     (gts-translator
      :picker (+gts-paragraph-picker)
      :engines (gts-google-engine)
      :render (gts-kill-ring-render))))

  (defun +gts-translate-with (&optional engine)
    (interactive)
    (gts-translate
     (gts-translator
      :picker (+gts-paragraph-picker)
      :engines
      (cond ((eq engine 'deepl)
             (gts-deepl-engine
              :auth-key ;; Get API key from ~/.authinfo.gpg (machine api-free.deepl.com)
              (funcall
               (plist-get (car (auth-source-search :host "api-free.deepl.com" :max 1))
                          :secret))
              :pro nil))
            ((eq engine 'bing) (gts-bing-engine))
            (t (gts-google-engine)))
      :render (gts-buffer-render)))))


;; Add this to .dir-locals.el
;; ((nil (eglot-workspace-configuration
;;        . ((ltex . ((language . "fr")
;;                    (disabledRules . ((fr . ["FRENCH_WHITESPACE"])))
;;                    (additionalRules . ((languageModel . "/usr/share/ngrams/")))))))))
(with-eval-after-load 'eglot
  (add-to-list
   'eglot-server-programs
   '((org-mode latex-mode LaTeX-mode markdown-mode) . ("ltex-ls"))))


(provide 'me-spell)
