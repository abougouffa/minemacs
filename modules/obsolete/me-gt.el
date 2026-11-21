;;; me-gt.el --- Online translators -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2026-11-13
;; Last modified: 2026-11-21

;;; Commentary:

;;; Code:


;; Translator on Emacs via multiple engines (Google, Bing, deepL, StarDict and Youdao) and LLMs (ChatGPT, DeepSeek, etc).
(use-package gt
  :ensure t
  :config
  (advice-add
   'gt-make-completion-table :override
   (lambda (items &optional order)
     (lambda (input pred action)
       (if (eq action 'metadata)
           `(metadata (category . gt) ; BUGFIX: This was a string which triggers: vertico--debug((wrong-type-argument symbolp "gt"))
                      (display-sort-function . ,(or order #'identity)))
         (complete-with-action action items input pred))))))


(provide 'obsolete/me-gt)
;;; me-gt.el ends here
