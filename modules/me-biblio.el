;;; me-biblio.el --- Bibliography -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package citar
  :straight t
  :after minemacs-first-org-file oc
  :demand
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
       (satch-defun +citar--set-symbols-once-h ()
         (when (display-graphic-p)
           (+citar--set-symbols)
           (remove-hook 'server-after-make-frame-hook
                        #'+citar--set-symbols-once-h)))))))

(use-package citar-embark
  :straight t
  :after citar embark
  :demand
  :config
  (citar-embark-mode 1))

(use-package org-re-reveal-citeproc
  :straight t
  :unless (+package-disabled-p 'org-re-reveal 'me-org))


(provide 'me-biblio)

;;; me-biblio.el ends here
