;;; me-write-mode.el --- Simple writing-centered mode -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(require 'visual-fill-column)
(require 'mixed-pitch nil t)

(defcustom +writing-mode-enable-hook nil
  "Execute hooks on enable."
  :group 'minemacs-ui
  :type 'hook)

(defcustom +writing-mode-disable-hook nil
  "Execute hooks on disable."
  :group 'minemacs-ui
  :type 'hook)

(defcustom +writing-text-scale 2.0
  "The text-scaling level for `+writing-mode'."
  :group 'minemacs-ui
  :type 'float)

(defcustom +writing-text-width nil
  "Like `visual-fill-column-width'."
  :group 'minemacs-ui
  :type '(choice
          (const :tag "Use `fill-column'" :value nil)
          (integer :tag "Specify width" :value 70)))

(defcustom +writing-mixed-pitch-enable t
  "Enable `mixed-pitch-mode' with `+writing-mode' for some modes defined in `+writing-mixed-pitch-modes'."
  :group 'minemacs-ui
  :type 'boolean)

(defcustom +writing-mixed-pitch-modes
  '(rst-mode markdown-mode org-mode)
  "Enable `mixed-pitch-mode' with `+writing-mode' for these modes."
  :group 'minemacs-ui
  :type '(repeat symbol))

(defvar-local +writing--line-nums-active-p nil)
(defvar-local +writing--org-format-latex-scale nil)

(defun +writing--scale-up-org-latex ()
  (setq-local
   +writing--org-format-latex-scale
   (plist-get org-format-latex-options :scale)
   org-format-latex-options
   (plist-put
    org-format-latex-options
    :scale
    (* ;; The scale from current font
     (/ (float (or (face-attribute 'default :height) 100)) 100.0)
     ;; Proportional upscaling
     (/ +writing-text-scale (if (+emacs-features-p 'pgtk) 1.8 1.4))))))

(defun +writing--scale-down-org-latex ()
  (setq-local
   org-format-latex-options
   (plist-put org-format-latex-options
              :scale (or +writing--org-format-latex-scale 1.0))))

;;;###autoload
(define-minor-mode +writing-mode
  "A mode for writing without distraction."
  :init-value nil :lighter "Zen" :global nil
  (let ((mixed-pitch-mode-p (seq-filter #'derived-mode-p +writing-mixed-pitch-modes)))
    (if +writing-mode
        ;; Enable
        (progn
          (setq-local visual-fill-column-center-text t
                      visual-fill-column-width +writing-text-width)
          (when (and mixed-pitch-mode-p (bound-and-true-p display-line-numbers-mode))
            (setq-local +writing--line-nums-active-p display-line-numbers-type)
            (display-line-numbers-mode -1))
          (when (derived-mode-p 'org-mode) (+writing--scale-up-org-latex))
          (run-hooks +writing-mode-enable-hook))
      ;; Disable
      (kill-local-variable 'visual-fill-column-center-text)
      (kill-local-variable 'visual-fill-column-width)
      (when (derived-mode-p 'org-mode) (+writing--scale-down-org-latex))
      (when (and +writing--line-nums-active-p mixed-pitch-mode-p)
        (display-line-numbers-mode +writing--line-nums-active-p)))

    (visual-fill-column-mode (if +writing-mode 1 -1))

    (when (fboundp 'mixed-pitch-mode)
      (mixed-pitch-mode (if (and +writing-mode mixed-pitch-mode-p +writing-mixed-pitch-enable) 1 -1)))

    (when (/= +writing-text-scale 0.0)
      (text-scale-set (if +writing-mode +writing-text-scale 0.0))
      (visual-fill-column-adjust))))


(provide 'me-writing-mode)

;;; me-writing-mode.el ends here
