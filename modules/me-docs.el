;;; me-docs.el --- Documents (PDF, EPUB, DOC...) -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")


(use-package doc-view
  :straight (:type built-in)
  :custom
  (doc-view-mupdf-use-svg (+emacs-features-p 'rsvg)))

(use-package pdf-tools
  :straight t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-view-image-relief 2)
  (pdf-view-use-scaling t))

(use-package nov
  :straight t
  :mode ("\\.epub\\'" . nov-mode)
  :custom
  (nov-save-place-file (concat minemacs-local-dir "nov/save-place.el"))
  :config
  (+nmap! :keymaps 'nov-mode-map
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

(use-package crdt
  :straight t
  :preface
  (defconst +tuntox-available-p (executable-find "tuntox"))
  (defconst +stunnel-available-p (executable-find "stunnel"))
  :when (or +tuntox-available-p +stunnel-available-p)
  :init
  (cond (+tuntox-available-p
         (setq crdt-use-tuntox t
               crdt-tuntox-password-in-url t))
        (+stunnel-available-p
         (setq crdt-use-stunnel t))))

(defconst +easydraw-available-p (+emacs-features-p 'rsvg 'zlib 'libxml2))

(use-package edraw
  :straight (:host github :repo "misohena/el-easydraw")
  :when +easydraw-available-p)

(use-package edraw-org
  :hook (org-mode . edraw-org-setup-default)
  :when +easydraw-available-p)

(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :custom
  (markdown-hide-markup t)
  (markdown-enable-math t))


(provide 'me-docs)
