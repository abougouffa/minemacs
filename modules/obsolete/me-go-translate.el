;;; me-go-translate.el --- Translate -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package go-translate
  :straight (:host github :repo "lorniu/go-translate")
  :commands (+gts-yank-translated-region +gts-translate-with)
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


(provide 'obsolete/me-go-translate)
;;; me-go-translate.el ends here
