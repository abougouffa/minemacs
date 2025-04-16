;;; me-natural-langs.el --- Natural languages stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;; Just-in-time spell checker based on the Enchanted library
(use-package jinx
  :straight t
  :when (+emacs-options-p 'modules)
  :autoload jinx--load-module
  :preface
  (defvar-local +spellcheck-mode nil)
  (defun +spellcheck-mode (&optional arg)
    "Spell checking mode, with ARG.
Based on `jinx-mode' if available. Falls back to the built-in
`flyspell-mode'."
    (interactive (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg) 'toggle)))
    (if (and (fboundp 'jinx-mode) (+jinx-load-module))
        (progn
          (jinx-mode arg)
          (setq +spellcheck-mode jinx-mode))
      (flyspell-mode arg)
      (setq +spellcheck-mode flyspell-mode)))

  (defun +spellcheck-correct ()
    "Correct word at point."
    (interactive)
    (cond ((bound-and-true-p jinx-mode)
           (call-interactively 'jinx-correct))
          ((and (bound-and-true-p flyspell-mode))
           (call-interactively 'flyspell-correct-wrapper))
          (t (user-error "No usable `jinx' nor `flyspell-correct'"))))

  (with-eval-after-load 'git-commit (add-hook 'git-commit-mode-hook #'+spellcheck-mode))
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


;; Distraction-free words correction with `flyspell' via `completing-read'
(use-package flyspell-correct
  :straight t)


;; Fancy Emacs integration with the console version of StarDict
(use-package lexic
  :straight t
  :when (executable-find "sdcv"))


;; Emacs client for www.reverso.net for translation, grammar check, context and synonyms search
(use-package reverso
  :straight (:host github :repo "SqrtMinusOne/reverso.el"))


;; Internal package to add support for LTeX-LS specific commands to `eglot'
(use-package me-eglot-ltex
  :after eglot
  :demand
  :config
  (eglot-ltex-enable-handling-client-commands)
  (defvar +ltex-ls-plus-path (expand-file-name "ltex-ls-plus/" minemacs-local-dir))
  (defvar +ltex-ls-plus-bin (expand-file-name "bin/ltex-ls-plus" +ltex-ls-plus-path))
  (defun +ltex-ls-plus-download (pre)
    "Download the latest release of \"ltex-ls-plus\".

When PRE is non-nil, allow downloading the latest prerelease."
    (interactive "P")
    (if (zerop
         (when-let* ((tarball (+github-download-release
                               "ltex-plus/ltex-ls-plus"
                               (concat "-" (cond ((+emacs-options-p 'os/linux) "linux")
                                                 ((+emacs-options-p 'os/mac) "mac")
                                                 ((+emacs-options-p 'os/win) "windows"))
                                       "-" (cond ((+emacs-options-p 'arch/x86_64) "x64")
                                                 ((+emacs-options-p 'arch/x86_64) "aarch64")))
                               nil :prerelease pre)))
           (when (file-directory-p +ltex-ls-plus-path)
             (delete-directory +ltex-ls-plus-path t))
           (mkdir +ltex-ls-plus-path)
           (let ((compilation-buffer-name-function (lambda (_a) "*ltex-ls-plus:install*")))
             (compile (format "tar -C %S -xf %S --strip-components=2" +ltex-ls-plus-path tarball)))))
        (message "Downloaded successfully!")
      (user-error "A problem occured when trying to download ltex-ls-plus")))
  (+eglot-register
    '(text-mode org-mode markdown-mode markdown-ts-mode rst-mode git-commit-mode)
    `(,+ltex-ls-plus-bin "--server-type=TcpSocket" "--port" :autoport))
  (+eglot-register
    '(tex-mode context-mode texinfo-mode bibtex-mode)
    "digestif"
    "texlab"
    `(,+ltex-ls-plus-bin "--server-type=TcpSocket" "--port" :autoport)))


(provide 'me-natural-langs)

;;; me-natural-langs.el ends here
