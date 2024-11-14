;;; me-natural-langs.el --- Natural languages stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;; Just-in-time spell checker based on the Enchanted library
(use-package jinx
  :straight t
  :when (+emacs-options-p 'modules)
  :autoload jinx--load-module
  :init
  (defun +jinx-load-module ()
    "Try to compile and load the jinx module and fail silently."
    (condition-case err
        (let ((display-buffer-alist ; Hide the compilation buffer
               (cons '("\\*jinx module compilation\\*"
                       (display-buffer-no-window)
                       (allow-no-window . t))
                     display-buffer-alist)))
          (jinx--load-module))
      (error (+log! (error-message-string err)) nil)
      (:success t))))

(defun +spellcheck-correct ()
  "Correct word at point."
  (interactive)
  (cond ((bound-and-true-p jinx-mode) (call-interactively #'jinx-correct))
        ((bound-and-true-p spell-fu-mode) (call-interactively #'+spell-fu-correct))))

(defun +spellcheck-mode (&optional arg)
  "Spell checking mode, with ARG.
Based on `jinx-mode' if available, `spell-fu-mode' and falls back
to built-in `flyspell-mode'."
  (interactive (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg) 'toggle)))
  (cond ((and (fboundp 'jinx-mode) (+jinx-load-module))
         (jinx-mode arg))
        ((and (+load minemacs-obsolete-modules-dir "me-spell-fu.el") (fboundp 'spell-fu-mode))
         (spell-fu-mode arg))
        (t ; Fallback to builtin `flyspell'
         (flyspell-mode arg))))

(with-eval-after-load 'git-commit
  (add-hook 'git-commit-mode-hook #'+spellcheck-mode))


;; Fancy Emacs integration with the console version of StarDict
(use-package lexic
  :straight t
  :when (executable-find "sdcv"))


;; Emacs client for www.reverso.net for translation, grammar check, context and synonyms search
(use-package reverso
  :straight (:host github :repo "SqrtMinusOne/reverso.el"))


;; Add this to .dir-locals.el
;; ((nil (eglot-workspace-configuration
;;        . ((ltex . ((language . "fr")
;;                    (disabledRules . ((fr . ["FRENCH_WHITESPACE"])))
;;                    (additionalRules . ((languageModel . "/usr/share/ngrams/")))))))))
(use-package me-eglot-ltex
  :after eglot
  :demand
  :config
  (eglot-ltex-enable-handling-client-commands)
  (+eglot-register
    '(text-mode org-mode markdown-mode markdown-ts-mode rst-mode git-commit-mode)
    '("ltex-ls" "--server-type=TcpSocket" "--port" :autoport))
  (+eglot-register
    '(tex-mode context-mode texinfo-mode bibtex-mode)
    "digestif"
    "texlab"
    '("ltex-ls" "--server-type=TcpSocket" "--port" :autoport)))


(provide 'me-natural-langs)

;;; me-natural-langs.el ends here
