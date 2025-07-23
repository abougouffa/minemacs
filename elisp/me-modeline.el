;;; minemacs-modeline.el --- Code for my custom mode line -*- lexical-binding: t -*-

;; Copyright (C) 2023-2025  Protesilaos Stavrou

;; Based on: https://protesilaos.com/emacs/dotemacs
;; By: Protesilaos Stavrou <info@protesilaos.com>
;; SPDX-License-Identifier: GPL-3.0

;;; Code:

(require 'nerd-icons)

(defgroup me-modeline nil
  "Custom modeline that is stylistically close to the default."
  :group 'mode-line)

(defgroup me-modeline-faces nil
  "Faces for my custom modeline."
  :group 'me-modeline)

;;;; Faces

(defface me-modeline-indicator-button nil
  "Generic face used for indicators that have a background.
Modify this face to, for example, add a :box attribute to all relevant
indicators.")

(defface me-modeline-indicator-green-bg
  '((default :inherit (bold me-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#207b20" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#77d077" :foreground "black")
    (t :background "green" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'me-modeline-faces)

;;;; Keyboard macro indicator

(defvar-local me-modeline-kbd-macro
  '(:eval
    (when (and (mode-line-window-selected-p) defining-kbd-macro)
      (concat " " (+nerd-icons-icon "nf-md-record_rec")))))

;;;; Narrow indicator

(defvar-local me-modeline-narrow
  '(:eval
    (when (and (mode-line-window-selected-p)
               (buffer-narrowed-p)
               (not (derived-mode-p 'Info-mode 'help-mode 'special-mode 'message-mode)))
      (concat " " (+nerd-icons-icon "nf-md-arrow_collapse_vertical")))))

(defvar-local me-modeline-multiple-cursors
  '(:eval
    (concat
     (when (bound-and-true-p multiple-cursors-mode)
       (concat (+nerd-icons-icon "nf-fa-i_cursor" :face 'nerd-icons-blue)
               (propertize (format " %d " (mc/num-cursors)) 'face 'nerd-icons-blue)))
     (when (and (bound-and-true-p iedit-mode) (bound-and-true-p iedit-occurrences-overlays))
       (concat (+nerd-icons-icon "nf-fa-i_cursor" :face 'nerd-icons-green)
               (propertize (format " %d " (length iedit-occurrences-overlays)) 'face 'nerd-icons-green))))))

;;;; Input method

(defvar-local me-modeline-input-method
  '(:eval
    (when current-input-method-title
      (propertize (format " %s " current-input-method-title)
                  'face 'me-modeline-indicator-green-bg
                  'mouse-face 'mode-line-highlight))))

;;;; Buffer status

;; TODO 2023-07-05: What else is there beside remote files?  If
;; nothing, this must be renamed accordingly.
(defvar-local me-modeline-buffer-status
  '(:eval
    (when-let* ((method (file-remote-p default-directory 'method))
                (icon (pcase method
                        ("ssh" "nf-md-ssh")
                        ("adb" "nf-dev-android")
                        ("mtp" "nf-fa-mobile_phone")
                        ("gdrive" "nf-fa-google")
                        ("davs" "nf-fa-globe")
                        ("nextcloud" "nf-fa-cloud")
                        ("kubernetes" "nf-dev-kubernetes")
                        ((rx (or "sudo" "su" "doas" "sudoedit")) "nf-md-pound_box")
                        ((rx (or "docker" "dockercp")) "nf-fa-docker")
                        ((rx (or "podman" "podmancp")) "nf-dev-podman")
                        (t "nf-md-folder_network_outline"))))
      (concat " " (+nerd-icons-icon icon)))))

;;;; Dedicated window

(defvar-local me-modeline-window-dedicated-status
  '(:eval (when (window-dedicated-p) (concat " " (+nerd-icons-icon "nf-md-equal_box")))))

;;;; Buffer name and modified status

(defun me-modeline-buffer-identification-face ()
  "Return appropriate face or face list for `me-modeline-buffer-identification'."
  (let ((file (buffer-file-name)))
    (cond
     ((and (mode-line-window-selected-p) file (buffer-modified-p))
      '(error italic mode-line-buffer-id))
     ((and file (buffer-modified-p))
      'italic)
     ((mode-line-window-selected-p)
      'mode-line-buffer-id))))

(defun me-modeline-buffer-name-help-echo ()
  "Return `help-echo' value for `me-modeline-buffer-identification'."
  (concat
   (propertize (buffer-name) 'face 'mode-line-buffer-id)
   "\n"
   (propertize
    (or (buffer-file-name)
        (format "No underlying file.\nDirectory is: %s" default-directory))
    'face 'font-lock-doc-face)))

(defvar-local me-modeline-buffer-identification
  '(:eval
    (concat (me-modeline-file-icon)
            (and buffer-read-only (concat " " (+nerd-icons-icon "nf-fa-lock")))
            " "
            (propertize (buffer-name)
                        'face (me-modeline-buffer-identification-face)
                        'mouse-face 'mode-line-highlight
                        'help-echo (me-modeline-buffer-name-help-echo)))))

;;;; Major mode

(defun me-modeline-file-icon ()
  "Return appropriate propertized mode line indicator for the major mode."
  (cond ((buffer-file-name)
         (nerd-icons-icon-for-file (buffer-file-name)))
        ((and (bound-and-true-p dired-mode) dired-directory)
         (nerd-icons-icon-for-dir dired-directory))
        (t (nerd-icons-icon-for-mode major-mode))))

(defvar-local me-modeline-major-mode
  '(:eval
    (concat
     (propertize
      (capitalize (string-replace "-mode" "" (symbol-name major-mode)))
      'mouse-face 'mode-line-highlight
      'help-echo (if-let* ((parent (get major-mode 'derived-mode-parent)))
                     (format "Symbol: `%s'.  Derived from: `%s'" major-mode parent)
                   (format "Symbol: `%s'." major-mode))))))

(defvar-local me-modeline-process
  (list '("" mode-line-process)))

;;;; Git branch and diffstat

(declare-function vc-git--symbolic-ref "vc-git" (file))

(defun me-modeline--vc-branch-name (file backend)
  "Return capitalized VC branch name for FILE with BACKEND."
  (when-let* ((rev (vc-working-revision file backend))
              (branch (or (vc-git--symbolic-ref file)
                          (substring rev 0 7))))
    branch))

(defvar me-modeline-vc-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'vc-diff)
    (define-key map [mode-line down-mouse-3] 'vc-root-diff)
    map))

(defun me-modeline--vc-help-echo (file)
  "Return `help-echo' message for FILE tracked by VC."
  (format "Revision: %s\nmouse-1: `vc-diff'\nmouse-3: `vc-root-diff'"
          (vc-working-revision file)))

(defun me-modeline--vc-text (file branch &optional face)
  "Prepare text for Git controlled FILE, given BRANCH.
With optional FACE, use it to propertize the BRANCH."
  (concat
   (+nerd-icons-icon "nf-fa-code_branch" :face 'shadow)
   " "
   (propertize branch
               'face face
               'mouse-face 'mode-line-highlight
               'help-echo (me-modeline--vc-help-echo file)
               'local-map me-modeline-vc-map)))

(defun me-modeline--vc-details (file branch &optional face)
  "Return Git BRANCH details for FILE, truncating it if necessary.
The string is truncated if the width of the window is smaller
than `split-width-threshold'."
  (me-modeline--vc-text file branch face))

(defvar me-modeline--vc-faces
  '((added . vc-locally-added-state)
    (edited . vc-edited-state)
    (removed . vc-removed-state)
    (missing . vc-missing-state)
    (conflict . vc-conflict-state)
    (locked . vc-locked-state)
    (up-to-date . vc-up-to-date-state)))

(defun me-modeline--vc-get-face (key)
  "Get face from KEY in `me-modeline--vc-faces'."
  (alist-get key me-modeline--vc-faces 'vc-up-to-date-state))

(defun me-modeline--vc-face (file backend)
  "Return VC state face for FILE with BACKEND."
  (when-let* ((key (vc-state file backend)))
    (me-modeline--vc-get-face key)))

(defvar-local me-modeline-vc-branch
  '(:eval
    (when-let* (((mode-line-window-selected-p))
                (file (or buffer-file-name default-directory))
                (backend (or (vc-backend file) 'Git))
                ;; ((vc-git-registered file))
                (branch (me-modeline--vc-branch-name file backend))
                (face (me-modeline--vc-face file backend)))
      (me-modeline--vc-details file branch face))))

;;;; Flymake errors, warnings, notes

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
     (when-let* ((count (me-modeline-flymake-counter
                         ,(intern (format ":%s" type)))))
       (concat
        " "
        (+nerd-icons-icon ,icon :face ',(or face type))
        " "
        (propertize count
                    'face ',(or face type)
                    'mouse-face 'mode-line-highlight
                    'local-map me-modeline-flymake-map
                    'help-echo "mouse-1: buffer diagnostics\nmouse-3: project diagnostics")))))

(me-modeline-flymake-type error "nf-cod-error")
(me-modeline-flymake-type warning "nf-cod-warning")
(me-modeline-flymake-type note "nf-oct-info" success)

(defvar-local me-modeline-flymake
  `(:eval
    (when (and (mode-line-window-selected-p) (bound-and-true-p flymake-mode))
      (list ; See the calls to the macro `me-modeline-flymake-type'
       '(:eval (me-modeline-flymake-error))
       '(:eval (me-modeline-flymake-warning))
       '(:eval (me-modeline-flymake-note))))))

;;;; Eglot

(with-eval-after-load 'eglot
  (setq mode-line-misc-info
        (delete '(eglot--managed-mode (" [" eglot--mode-line-format "] ")) mode-line-misc-info)))

(defvar-local me-modeline-eglot
  `(:eval
    (when (and (mode-line-window-selected-p) (featurep 'eglot))
      '(eglot--managed-mode eglot--mode-line-format))))

;;;; Miscellaneous

(defvar-local me-modeline-misc-info
  '(:eval
    (when (mode-line-window-selected-p) mode-line-misc-info)))

;;;; Risky local variables

;; NOTE 2023-04-28: The `risky-local-variable' is critical, as those variables
;; will not work without it.
(dolist (construct '(me-modeline-kbd-macro
                     me-modeline-narrow
                     me-modeline-input-method
                     me-modeline-buffer-status
                     me-modeline-multiple-cursors
                     me-modeline-window-dedicated-status
                     me-modeline-buffer-identification
                     me-modeline-major-mode
                     me-modeline-process
                     me-modeline-vc-branch
                     me-modeline-flymake
                     me-modeline-eglot
                     me-modeline-misc-info))
  (put construct 'risky-local-variable t))

(provide 'me-modeline)
;;; me-modeline.el ends here
