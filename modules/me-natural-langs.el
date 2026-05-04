;;; me-natural-langs.el --- Natural languages stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-10-02
;; Last modified: 2026-05-04

;;; Commentary:

;;; Code:


;; Just-in-time spell checker based on the Enchanted library
(use-package jinx
  :straight t
  :when (featurep 'feat/modules)
  :autoload jinx--load-module
  :hook (text-mode . +spellcheck-mode)
  :init
  (defun +jinx-load-module ()
    "Try to compile and load the jinx module and fail silently."
    (condition-case err
        (let ((display-buffer-alist ; Hide the compilation buffer
               (cons '("\\*jinx module compilation\\*" (display-buffer-no-window) (allow-no-window . t))
                     display-buffer-alist)))
          (jinx--load-module))
      (error (+log! (error-message-string err)) nil)
      (:success t))))


;; Distraction-free words correction with `flyspell' via `completing-read'
(use-package flyspell-correct
  :straight t)


;; Translator on Emacs via multiple engines (Google, Bing, deepL, StarDict and Youdao) and LLMs (ChatGPT, DeepSeek, etc).
(use-package gt
  :straight t
  :config
  (advice-add
   'gt-make-completion-table :override
   (lambda (items &optional order)
     (lambda (input pred action)
       (if (eq action 'metadata)
           `(metadata (category . gt) ; BUGFIX: This was a string which triggers: vertico--debug((wrong-type-argument symbolp "gt"))
                      (display-sort-function . ,(or order #'identity)))
         (complete-with-action action items input pred))))))


(provide 'me-natural-langs)

;;; me-natural-langs.el ends here
