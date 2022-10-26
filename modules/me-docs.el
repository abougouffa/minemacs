;;; docs.el --- Documents (PDF, EPUB, DOC...) -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

(use-package pdf-tools
  :straight t
  :magic ("%PDF" . pdf-view-mode)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install t)
  (setq-default pdf-view-display-size 'fit-page
                pdf-view-image-relief 2
                pdf-view-use-scaling t))


(use-package nov
  :straight t
  :mode ("\\.epub\\'" . nov-mode)
  :custom
  (nov-save-place-file (expand-file-name "nov-places" minemacs-local-dir))
  :config
  (+map-key :keymaps 'nov-mode-map :states 'normal
    "RET" #'nov-scroll-up)

  (defun doom-modeline-segment--nov-info ()
    (concat " " (propertize (cdr (assoc 'creator nov-metadata))
                            'face 'doom-modeline-project-parent-dir)
            " " (cdr (assoc 'title nov-metadata))
            " " (propertize (format "%d/%d" (1+ nov-documents-index)
                                    (length nov-documents))
                            'face 'doom-modeline-info)))

  (advice-add 'nov-render-title :override #'ignore)

  (defun +nov-mode-setup ()
    (face-remap-add-relative 'variable-pitch
                             :family "Merriweather"
                             :height 1.4
                             :width 'semi-expanded)
    (face-remap-add-relative 'default :height 1.3)
    (setq-local line-spacing 0.2
                next-screen-context-lines 4
                shr-use-colors nil)
    (require 'visual-fill-column nil t)
    (setq-local visual-fill-column-center-text t
                visual-fill-column-width 80
                nov-text-width 80)
    (visual-fill-column-mode 1)
    (hl-line-mode -1)

    (setq-local
     mode-line-format
     `((:eval
        (doom-modeline-segment--workspace-name))
       (:eval
        (doom-modeline-segment--window-number))
       (:eval
        (doom-modeline-segment--nov-info))
       ,(propertize
         " %P "
         'face 'doom-modeline-buffer-minor-mode)
       ,(propertize
         " "
         'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive)
         'display `((space
                     :align-to
                     (- (+ right right-fringe right-margin)
                      ,(* (let ((width (doom-modeline--font-width)))
                           (or (and (= width 1) 1)
                            (/ width (frame-char-width) 1.0)))
                        (string-width
                         (format-mode-line
                          (cons ""
                           '(:eval (doom-modeline-segment--major-mode))))))))))
       (:eval (doom-modeline-segment--major-mode)))))

  (add-hook 'nov-mode-hook #'+nov-mode-setup))


(use-package graphviz-dot-mode
  :straight t
  :defer t
  :config
  (with-eval-after-load 'eglot
    (add-to-list
     'eglot-server-programs
     '(graphviz-dot-mode . ("dot-language-server" "--stdio")))))


(use-package xenops
  :straight t
  :hook ((latex-mode LaTeX-mode) . xenops-mode))


(provide 'me-docs)
