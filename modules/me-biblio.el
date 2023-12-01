;;; me-biblio.el --- Bibliography -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package zotxt
  :straight t
  :when (executable-find "zotero")
  :init
  (+map-local! :keymaps 'org-mode-map
    "z" #'org-zotxt-mode)
  (+map-local! :keymaps 'markdown-mode-map
    "z" #'zotxt-citekey-mode))

(use-package citar
  :straight t
  :after minemacs-first-org-file oc
  :demand t
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-symbol-separator "  ")
  :config
  (with-eval-after-load 'nerd-icons
    (defun +citar--set-symbols ()
      (setq citar-symbols
            `((file ,(nerd-icons-codicon "nf-cod-file_pdf" :face 'error) . " ")
              (note ,(nerd-icons-faicon "nf-fa-file_text" :face 'warning) . " ")
              (link ,(nerd-icons-mdicon "nf-md-link" :face 'org-link) . " "))))

    ;; Properly setup citar-symbols
    (if (display-graphic-p)
        (+citar--set-symbols)
      (add-hook
       'server-after-make-frame-hook
       (defun +citar--set-symbols-once-h ()
         (when (display-graphic-p)
           (+citar--set-symbols)
           (remove-hook 'server-after-make-frame-hook
                        #'+citar--set-symbols-once-h)))))))

(use-package citar-embark
  :straight t
  :after citar embark
  :demand t
  :config
  (citar-embark-mode 1))

(use-package org-re-reveal-citeproc
  :straight t
  :when (and (memq 'me-org minemacs-modules) (not (memq 'org-re-reveal minemacs-disabled-packages))))


(provide 'me-biblio)

;;; me-biblio.el ends here
