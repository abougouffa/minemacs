;;; me-modeline.el --- Light, modern and opinionated mode-line for MinEmacs -*- lexical-binding: t -*-

;; Copyright (C) 2023-2025  Protesilaos Stavrou
;; Copyright (C) 2025  Abdelhak Bougouffa

;; Based on: https://protesilaos.com/emacs/dotemacs
;; By: Protesilaos Stavrou <info@protesilaos.com>
;; SPDX-License-Identifier: GPL-3.0

;;; Commentary:

;;; Code:

(require 'nerd-icons)
(require 'me-lib)

(defgroup me-modeline nil
  "Custom modeline that is stylistically close to the default."
  :group 'mode-line)

(defgroup me-modeline-faces nil
  "Faces for my custom modeline."
  :group 'me-modeline)

;;; Faces

(defface me-modeline-indicator-button nil
  "Generic face used for indicators that have a background.
Modify this face to, for example, add a :box attribute to all relevant
indicators.")

(defface me-modeline-inverse-video-face '((t (:inverse-video t)))
  "Inverse video face."
  :group 'me-modeline-faces)

;;; Keyboard macro indicator

(defvar-local me-modeline-kbd-macro
  '(:eval
    (when (and (mode-line-window-selected-p) defining-kbd-macro
               (not (bound-and-true-p kmacro-x-mc-mode))) ;; `kmacro-x-mc' is handled as multiple cursors
      (concat " " (+nerd-icons-icon "nf-md-record_rec")))))

;;; Narrow indicator

(defvar-local me-modeline-narrow
  '(:eval
    (when (and (mode-line-window-selected-p)
               (buffer-narrowed-p)
               (not (derived-mode-p 'Info-mode 'help-mode 'special-mode 'message-mode 'archive-mode)))
      (concat " " (+nerd-icons-icon "nf-md-arrow_collapse_vertical")))))

(defvar-local me-modeline-multiple-cursors
  '(:eval
    (when-let* ((count-face
                 (cond ((and (or (bound-and-true-p iedit-mode) (bound-and-true-p iedit-rectangle-mode))
                             (bound-and-true-p iedit-occurrences-overlays))
                        (cons (length iedit-occurrences-overlays) 'nerd-icons-purple))
                       ((bound-and-true-p iedit-rectangle-mode) (bound-and-true-p iedit-occurrences-overlays))
                       ((bound-and-true-p kmacro-x-mc-mode)
                        (cons (length kmacro-x-mc-cursors) 'nerd-icons-red))
                       ((bound-and-true-p multiple-cursors-mode)
                        (cons (mc/num-cursors) 'nerd-icons-blue)))))
      (propertize (concat " " (+nerd-icons-icon "nf-fa-i_cursor") (format " %d " (car count-face)))
                  'face `(,(cdr count-face) me-modeline-inverse-video-face)))))

;;; Input method

(defvar-local me-modeline-input-method
  '(:eval
    (when current-input-method-title
      (propertize (format " %s " current-input-method-title)
                  'face '(nerd-icons-green me-modeline-inverse-video-face)
                  'mouse-face 'mode-line-highlight))))

;;; Buffer status

(defvar-local me-modeline-buffer-status
  '(:eval
    (concat
     (when overwrite-mode
       (concat " " (+nerd-icons-icon "nf-fa-pencil" :face 'nerd-icons-red)))
     (when-let* ((method (file-remote-p default-directory 'method))
                 (icon-face
                  (pcase method
                    ("ssh" "nf-md-ssh")
                    ("adb" "nf-dev-android")
                    ("mtp" "nf-fa-mobile_phone")
                    ("gdrive" "nf-fa-google")
                    ("davs" "nf-fa-globe")
                    ("nextcloud" "nf-fa-cloud")
                    ("kubernetes" "nf-dev-kubernetes")
                    ((rx (or "docker" "dockercp")) "nf-fa-docker")
                    ((rx (or "podman" "podmancp")) "nf-dev-podman")
                    ((rx (or "sudo" "su" "doas" "sudoedit")) '("nf-md-pound_box" . nerd-icons-red))
                    (t "nf-md-folder_network_outline")))
                 (icon (if (consp icon-face) (car icon-face) icon-face))
                 (face (if (consp icon-face) (cdr icon-face) 'nerd-icons-green)))
       (concat " " (+nerd-icons-icon icon :face face))))))

;;; Dedicated window

(defvar-local me-modeline-window-dedicated-status
  '(:eval (when (window-dedicated-p) (concat " " (+nerd-icons-icon "nf-oct-pin" :face 'nerd-icons-dred)))))

;;; Buffer name and modified status

(defvar-local me-modeline-buffer-identification
  '(:eval
    (concat (and buffer-read-only (concat (+nerd-icons-icon "nf-fa-lock")))
            " "
            (propertize
             (buffer-name)
             'face (let ((file (buffer-file-name)))
                     (cond ((and (mode-line-window-selected-p) file (buffer-modified-p)) '(error italic mode-line-buffer-id))
                           ((and file (buffer-modified-p)) 'italic)
                           ((mode-line-window-selected-p) 'mode-line-buffer-id)))
             'mouse-face 'mode-line-highlight
             'help-echo (concat
                         (propertize (buffer-name) 'face 'mode-line-buffer-id) "\n"
                         (propertize
                          (or (buffer-file-name) (format "No underlying file.\nDirectory is: %s" default-directory))
                          'face 'font-lock-doc-face))))))

;;; Major mode

(defvar-local me-modeline-major-mode-icon
  '(:eval
    (cond ((buffer-file-name)
           (nerd-icons-icon-for-file (buffer-file-name)))
          ((and (bound-and-true-p dired-mode) dired-directory)
           (nerd-icons-icon-for-dir dired-directory))
          (t (nerd-icons-icon-for-mode major-mode)))))

(defvar-local me-modeline-process
  (list '("" mode-line-process)))

;;; Git branch and diffstat

(declare-function vc-git--symbolic-ref "vc-git" (file))

(defvar me-modeline-vc-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'vc-diff)
    (define-key map [mode-line down-mouse-3] 'vc-root-diff)
    map))

(defvar me-modeline--vc-faces
  '((added . vc-locally-added-state)
    (edited . vc-edited-state)
    (removed . vc-removed-state)
    (missing . vc-missing-state)
    (conflict . vc-conflict-state)
    (locked . vc-locked-state)
    (up-to-date . vc-up-to-date-state)))

(defvar-local me-modeline-vc-branch
  '(:eval
    (when-let* (((mode-line-window-selected-p))
                (file (or buffer-file-name default-directory))
                ((not (file-remote-p file))) ;; Can be too slow for remote files
                (backend (or (vc-backend file) 'Git))
                (file-state (vc-state file backend))
                (face (alist-get file-state me-modeline--vc-faces 'vc-up-to-date-state))
                (rev (vc-working-revision file backend))
                (branch (or (vc-git--symbolic-ref file) (substring rev 0 7)))
                (help-echo-msg (format "Branch: %s\nRevision: %s\nmouse-1: `vc-diff'\nmouse-3: `vc-root-diff'" branch rev))
                (branch-trim (if-let* ((len (length branch))
                                       ((> len 15)))
                                 (concat "··" (substring branch (- len 15) len))
                               branch)))
      (concat
       (+nerd-icons-icon "nf-fa-code_branch" :face 'shadow)
       " "
       (propertize branch-trim
                   'face face
                   'mouse-face 'mode-line-highlight
                   'help-echo help-echo-msg
                   'local-map me-modeline-vc-map)))))

;;; Flymake errors, warnings, notes

(declare-function flymake--severity "flymake" (type))
(declare-function flymake-diagnostic-type "flymake" (diag))

;; Based on `flymake--mode-line-counter'.
(defun me-modeline-flymake-counter (type)
  "Compute number of diagnostics in buffer with TYPE's severity.
TYPE is usually keyword `:error', `:warning' or `:note'."
  (let ((count 0))
    (dolist (d (flymake-diagnostics))
      (when (= (flymake--severity type) (flymake--severity (flymake-diagnostic-type d)))
        (cl-incf count)))
    (when (cl-plusp count)
      (number-to-string count))))

(defvar me-modeline-flymake-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'flymake-show-buffer-diagnostics)
    (define-key map [mode-line down-mouse-3] 'flymake-show-project-diagnostics)
    map))

(defmacro me-modeline-flymake-type (type icon &optional face)
  "Return function that handles Flymake TYPE with stylistic ICON and FACE."
  `(defun ,(intern (format "me-modeline-flymake-%s" type)) ()
     (when-let* ((count (me-modeline-flymake-counter ,(intern (format ":%s" type)))))
       (concat
        " "
        (+nerd-icons-icon ,icon :face ',(or face type))
        " "
        (propertize count
                    'face ',(or face type)
                    'mouse-face 'mode-line-highlight
                    'local-map me-modeline-flymake-map
                    'help-echo "mouse-1: buffer diagnostics\nmouse-3: project diagnostics")))))

(me-modeline-flymake-type error "nf-cod-error" nerd-icons-red)
(me-modeline-flymake-type warning "nf-cod-warning" nerd-icons-orange)
(me-modeline-flymake-type note "nf-cod-info" nerd-icons-green)

(defvar-local me-modeline-flymake
  `(:eval
    (when (and (mode-line-window-selected-p) (bound-and-true-p flymake-mode))
      (list ; See the calls to the macro `me-modeline-flymake-type'
       '(:eval (me-modeline-flymake-error))
       '(:eval (me-modeline-flymake-warning))
       '(:eval (me-modeline-flymake-note))))))

;;; Eglot

(with-eval-after-load 'eglot
  (setq mode-line-misc-info
        (delete '(eglot--managed-mode (" [" eglot--mode-line-format "] ")) mode-line-misc-info)))

(defvar-local me-modeline-eglot
  `(:eval
    (when (and (mode-line-window-selected-p) (featurep 'eglot))
      '(eglot--managed-mode eglot--mode-line-format))))

;;; Miscellaneous

(defvar-local me-modeline-misc-info
  '(:eval
    (when (mode-line-window-selected-p) mode-line-misc-info)))

;;; Risky local variables

;; The `risky-local-variable' is critical, as those variables will not work
;; without it.
(dolist (construct '(me-modeline-kbd-macro
                     me-modeline-narrow
                     me-modeline-input-method
                     me-modeline-buffer-status
                     me-modeline-multiple-cursors
                     me-modeline-window-dedicated-status
                     me-modeline-buffer-identification
                     me-modeline-major-mode-icon
                     me-modeline-process
                     me-modeline-vc-branch
                     me-modeline-flymake
                     me-modeline-eglot
                     me-modeline-misc-info))
  (put construct 'risky-local-variable t))

(defvar me-modeline--mode-line-format-orig nil)

;;;###autoload
(define-minor-mode me-modeline-mode
  "MinEmacs' mode-line."
  :global t
  (if me-modeline-mode
      (progn
        (unless me-modeline--mode-line-format-orig
          (setq me-modeline--mode-line-format-orig (default-value 'mode-line-format)))
        (setq-default mode-line-format
                      '("%e"
                        me-modeline-multiple-cursors
                        me-modeline-kbd-macro
                        me-modeline-narrow
                        me-modeline-buffer-status
                        me-modeline-window-dedicated-status
                        me-modeline-input-method
                        "  "
                        me-modeline-major-mode-icon
                        me-modeline-buffer-identification
                        "  "
                        mode-line-position
                        "  "
                        me-modeline-process
                        "  "
                        me-modeline-eglot
                        "  "
                        mode-line-format-right-align
                        "  "
                        me-modeline-vc-branch
                        "  "
                        me-modeline-flymake
                        "  "
                        me-modeline-misc-info
                        "  "))
        (+subtle-mode-line)
        (add-hook 'server-after-make-frame-hook #'+subtle-mode-line)
        (add-hook 'enable-theme-functions #'+subtle-mode-line))
    (setq-default mode-line-format me-modeline--mode-line-format-orig)
    (remove-hook 'server-after-make-frame-hook #'+subtle-mode-line)
    (remove-hook 'enable-theme-functions #'+subtle-mode-line))
  (force-mode-line-update t))


(provide 'me-modeline)
;;; me-modeline.el ends here
