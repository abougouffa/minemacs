;;; me-natural-langs.el --- Natural languages stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package spell-fu
  :straight t
  :preface
  (defconst +aspell-available-p (executable-find "aspell"))
  :when +aspell-available-p
  :hook (text-mode . spell-fu-mode)
  :custom
  (spell-fu-directory (+directory-ensure minemacs-local-dir "spell-fu/"))
  :init
  (+map! "ts" #'spell-fu-mode)
  (+nvmap! "z=" #'+spell-fu-correct) ;; autoloaded from "me-spell-fu.el"

  (defcustom +spell-excluded-faces-alist
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
    "Faces in certain major modes that spell-fu will not spellcheck."
    :group 'minemacs-ui
    :type '(repeat (cons symbol (repeat face))))

  (add-hook
   'spell-fu-mode-hook
   (defun +spell-fu--init-excluded-faces-h ()
     "Set `spell-fu-faces-exclude' according to `+spell-excluded-faces-alist'."
     (when-let (excluded (cdr (cl-find-if #'derived-mode-p +spell-excluded-faces-alist :key #'car)))
       (setq-local spell-fu-faces-exclude excluded)))))

(use-package go-translate
  :straight (:host github :repo "lorniu/go-translate")
  :commands +gts-yank-translated-region +gts-translate-with
  :init
  (+map-local! :keymaps '(org-mode-map text-mode-map markdown-mode-map
                          tex-mode-map TeX-mode-map latex-mode-map LaTeX-mode-map)
    "t" '(nil :wk "translate")
    "tb" `(,(+cmdfy! (+gts-translate-with 'bing)) :wk "Translate with Bing")
    "td" `(,(+cmdfy! (+gts-translate-with 'deepl)) :wk "Translate with DeepL")
    "tg" `(,(+cmdfy! (+gts-translate-with 'google)) :wk "Translate with Google")
    "tr" #'+gts-yank-translated-region
    "tt" #'+gts-translate-with
    "tT" #'gts-do-translate)
  :custom
  ;; Your languages pairs
  (gts-translate-list '(("en" "fr")
                        ("en" "ar")
                        ("fr" "ar")
                        ("fr" "en")))
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
    (let* ((caption-pair (mapcar (lambda (pair) (cons (format "From %s to %s" (upcase (car pair)) (upcase (cadr pair))) pair)) gts-translate-list))
           (gts-translate-list (if (length= gts-translate-list 1)
                                   gts-translate-list
                                 (list (cdr (assoc (completing-read "Translate: " (mapcar #'car caption-pair)) caption-pair)))))
           (engine (or engine (intern (completing-read "Engine: " '(deepl google bing))))))
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
        :render (gts-buffer-render))))))

(use-package lexic
  :straight t
  :preface
  (defconst +sdcv-available-p (executable-find "sdcv"))
  :when +sdcv-available-p
  :init
  (+map! :infix "s"
    "l" #'lexic-search-word-at-point
    "L" #'lexic-search)
  :config
  (+nvmap! :keymaps 'lexic-mode-map
    "q" #'lexic-return-from-lexic
    "RET" #'lexic-search-word-at-point
    "a" #'outline-show-all
    "h" `(,(+cmdfy! (outline-hide-sublevels 3)) :wk "Hide sublevels")
    "o" #'lexic-toggle-entry
    "n" #'lexic-next-entry
    "N" `(,(+cmdfy! (lexic-next-entry t)) :wk "Last entry")
    "p" #'lexic-previous-entry
    "P" `(,(+cmdfy! (lexic-previous-entry t)) :wk "First entry")
    "E" `(,(+cmdfy!
            (lexic-return-from-lexic)
            (switch-to-buffer (lexic-get-buffer)))
          :wk "Expand")
    "M" `(,(+cmdfy!
            (lexic-return-from-lexic)
            (lexic-goto-lexic))
          :wk "Minimise")
    "C-p" #'lexic-search-history-backwards
    "C-n" #'lexic-search-history-forwards
    "/" `(,(+cmdfy! (call-interactively #'lexic-search)) :wk "Search")))

;; Add this to .dir-locals.el
;; ((nil (eglot-workspace-configuration
;;        . ((ltex . ((language . "fr")
;;                    (disabledRules . ((fr . ["FRENCH_WHITESPACE"])))
;;                    (additionalRules . ((languageModel . "/usr/share/ngrams/")))))))))
(use-package me-eglot-ltex
  :after eglot
  :demand t
  :config
  (eglot-ltex-enable-handling-client-commands)
  (+eglot-register
    '(text-mode org-mode markdown-mode rst-mode latex-mode bibtex-mode context-mode git-commit-mode)
    '("ltex-ls" "--server-type=TcpSocket" "--port" :autoport)))


(provide 'me-natural-langs)

;;; me-natural-langs.el ends here
