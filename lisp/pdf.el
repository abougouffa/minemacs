;; -*- lexical-binding: t; -*-

(use-package pdf-tools
  :straight t
  :magic ("%PDF" . pdf-view-mode)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install-noverify)
  (setq-default pdf-view-display-size 'fit-width
                pdf-view-image-relief 2
                pdf-view-use-scaling t))


(provide 'minemacs-pdf)
