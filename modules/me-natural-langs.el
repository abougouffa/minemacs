;;; me-natural-langs.el --- Natural languages stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package jinx
  :straight t
  :when (and (or os/linux os/bsd os/mac) (+emacs-features-p 'modules))
  :hook (text-mode . jinx-mode)
  :hook (minemacs-build-functions . jinx--load-module)
  :init
  (+map! "ts" #'jinx-mode)
  (+nvmap! "z=" #'jinx-correct))

(unless (or os/win (+emacs-features-p 'modules))
  (+load minemacs-modules-dir "obsolete/me-spell-fu.el"))

(use-package go-translate
  :straight (:host github :repo "lorniu/go-translate")
  :commands +gts-yank-translated-region +gts-translate-with
  :init
  (+map-local! :keymaps '(org-mode-map text-mode-map markdown-mode-map tex-mode-map TeX-mode-map latex-mode-map LaTeX-mode-map)
    "t" '(nil :wk "translate")
    "tb" `(,(+cmdfy! (+gts-translate-with 'bing)) :wk "Translate with Bing")
    "td" `(,(+cmdfy! (+gts-translate-with 'deepl)) :wk "Translate with DeepL")
    "tg" `(,(+cmdfy! (+gts-translate-with 'google)) :wk "Translate with Google")
    "tr" #'+gts-yank-translated-region
    "tt" #'+gts-translate-with
    "tT" #'gts-do-translate)
  :custom
  ;; Your languages pairs
  (gts-translate-list '(("en" "fr") ("en" "ar") ("fr" "ar") ("fr" "en")))
  :config
  ;; Config the default translator, which will be used by the command `gts-do-translate'
  (setq gts-default-translator (gts-translator
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
                (funcall (plist-get (car (auth-source-search :host "api-free.deepl.com" :max 1)) :secret))
                :pro nil))
              ((eq engine 'bing) (gts-bing-engine))
              (t (gts-google-engine)))
        :render (gts-buffer-render))))))

(use-package reverso
  :straight (:host github :repo "SqrtMinusOne/reverso.el"))

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
    '(text-mode org-mode markdown-mode rst-mode git-commit-mode)
    '("ltex-ls" "--server-type=TcpSocket" "--port" :autoport)))


(provide 'me-natural-langs)

;;; me-natural-langs.el ends here
