;;; me-natural-langs.el --- Natural languages stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-10-02
;; Last modified: 2025-07-11

;;; Commentary:

;;; Code:

;; Just-in-time spell checker based on the Enchanted library
(use-package jinx
  :straight t
  :when (featurep 'feat/modules)
  :autoload jinx--load-module
  :preface
  (define-minor-mode +spellcheck-mode
    "Spell checking mode, with ARG.
Based on `jinx-mode' if available. Falls back to the built-in
`flyspell-mode'."
    :init-value nil
    (let ((arg (if +spellcheck-mode 1 -1)))
      (if (and (fboundp 'jinx-mode) (+jinx-load-module)) (jinx-mode arg) (flyspell-mode arg))))

  (defun +spellcheck-correct ()
    "Correct word at point."
    (interactive)
    (cond ((bound-and-true-p jinx-mode)
           (call-interactively 'jinx-correct))
          ((and (bound-and-true-p flyspell-mode) (fboundp 'flyspell-correct-wrapper))
           (call-interactively 'flyspell-correct-wrapper))
          (t (user-error "No usable `jinx' nor `flyspell-correct'"))))

  (with-eval-after-load 'git-commit (add-hook 'git-commit-mode-hook #'+spellcheck-mode))
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


;; Fancy Emacs integration with the console version of StarDict
(use-package lexic
  :straight t
  :when (executable-find "sdcv"))


;; Emacs client for Reverso.net for translation, grammar check, context and synonyms search
(use-package reverso
  :straight (:host github :repo "SqrtMinusOne/reverso.el")
  :bind (:map minemacs-open-thing-map ("r" . reverso))
  :config
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'reverso--history))
  (reverso-history-mode 1))


(provide 'me-natural-langs)

;;; me-natural-langs.el ends here
