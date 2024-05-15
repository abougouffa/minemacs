;;; me-natural-langs.el --- Natural languages stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package jinx
  :straight t
  :when (+emacs-features-p 'modules)
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

(+load minemacs-obsolete-modules-dir "me-spell-fu.el")

(defun +spellcheck-correct ()
  "Correct word at point."
  (interactive)
  (cond ((bound-and-true-p jinx-mode) (call-interactively #'jinx-correct))
        ((bound-and-true-p spell-fu-mode) (call-interactively #'+spell-fu-correct))))

(defun +spellcheck-mode ()
  "Toggle spellchecking."
  (interactive)
  (cond ((and (fboundp 'jinx-mode) (+jinx-load-module))
         (jinx-mode (if (bound-and-true-p jinx-mode) -1 1)))
        ((and (fboundp 'spell-fu-mode))
         (spell-fu-mode (if (bound-and-true-p spell-fu-mode) -1 1)))
        (t ; Fallback to builtin `flyspell'
         (flyspell-mode (if (bound-and-true-p flyspell-mode) -1 1)))))

(+map! "ts" #'+spellcheck-mode)
(+nvmap! "z=" #'+spellcheck-correct)

(add-hook #'text-mode-hook #'+spellcheck-mode)

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
