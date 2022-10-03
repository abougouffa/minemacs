;; -*- lexical-binding: t; -*-

(use-package oc
  :defer t
  :config
  (setq org-cite-global-bibliography (ensure-list citar-bibliography)
        org-cite-export-processors '((latex biblatex) (t csl))
        org-support-shift-select t))


(use-package oc-biblatex
  :after oc)


(use-package oc-csl
  :after oc)


(use-package oc-natbib
  :after oc)


(use-package citar
  :straight t
  :config
  (setq org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar))


(use-package citar-embark
  :straight t
  :after citar embark
  :config
  (citar-embark-mode))


(provide 'me-biblio)
