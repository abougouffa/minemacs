;;; minemacs-modeline.el --- Light, modern and opinionated mode-line for MinEmacs -*- lexical-binding: t -*-

;; Copyright (C) 2023-2025  Protesilaos Stavrou
;; Copyright (C) 2025  Abdelhak Bougouffa

;; Based on: https://protesilaos.com/emacs/dotemacs
;; By: Protesilaos Stavrou <info@protesilaos.com>
;; SPDX-License-Identifier: GPL-3.0

;;; Commentary:

;;; Code:

(require 'nerd-icons)
(require 'me-lib)

(defconst minemacs-modeline--icon-alist
  (cl-loop for glyph-set in nerd-icons-glyph-sets
           for sym = (intern (format "nerd-icons/%s-alist" glyph-set))
           for name = (caar (symbol-value sym))
           collect (cons (nth 1 (string-split name "-"))
                         (intern (format "nerd-icons-%s" glyph-set)))))

(defun minemacs-modeline--icon (name &rest args)
  "Generic function to get icons by NAME, with ARGS."
  (if-let* ((variant (nth 1 (string-split name "-")))
            (fn (alist-get variant minemacs-modeline--icon-alist nil nil #'equal)))
      (apply fn (cons name args))
    (error "Cannot detect the function which provides %S" name)))

(defgroup minemacs-modeline nil
  "Custom modeline that is stylistically close to the default."
  :group 'mode-line)

(defvar-local minemacs-modeline-disabled-sections nil)

(defmacro minemacs-modeline-define-section (name &rest body)
  "Define section NAME with BODY in the :eval part."
  (declare (indent 1))
  (let ((var-sym (intern (format "minemacs-modeline-%s" (+unquote name)))))
    `(progn
       (defvar ,var-sym
         '(:eval (when (not (memq ',(+unquote name) minemacs-modeline-disabled-sections)) ,@body)))
       (put ',var-sym 'risky-local-variable t))))

;;; Faces

(defface minemacs-modeline-inverse-video-face '((t (:inverse-video t)))
  "Inverse video face."
  :group 'minemacs-modeline)

;;; Keyboard macro indicator

(minemacs-modeline-define-section kbd-macro
  (when (and (mode-line-window-selected-p) defining-kbd-macro
             (not (bound-and-true-p kmacro-x-mc-mode))) ;; `kmacro-x-mc' is handled as multiple cursors
    (concat " " (minemacs-modeline--icon "nf-md-record_rec"))))

;;; Narrow indicator

(minemacs-modeline-define-section narrow
  (when (and (mode-line-window-selected-p)
             (buffer-narrowed-p)
             (not (derived-mode-p 'Info-mode 'help-mode 'special-mode 'message-mode 'archive-mode)))
    (concat " " (minemacs-modeline--icon "nf-md-arrow_collapse_vertical"))))

(minemacs-modeline-define-section multiple-cursors
  (when-let* ((count-face
               (cond ((and (or (bound-and-true-p iedit-mode) (bound-and-true-p iedit-rectangle-mode))
                           (bound-and-true-p iedit-occurrences-overlays))
                      (cons (length iedit-occurrences-overlays) 'nerd-icons-purple))
                     ((bound-and-true-p iedit-rectangle-mode) (bound-and-true-p iedit-occurrences-overlays))
                     ((bound-and-true-p kmacro-x-mc-mode)
                      (cons (length kmacro-x-mc-cursors) 'nerd-icons-red))
                     ((bound-and-true-p multiple-cursors-mode)
                      (cons (mc/num-cursors) 'nerd-icons-blue)))))
    (propertize (concat " " (minemacs-modeline--icon "nf-fa-i_cursor") (format " %d " (car count-face)))
                'face `(,(cdr count-face) minemacs-modeline-inverse-video-face))))

;;; Input method

(minemacs-modeline-define-section input-method
  (when current-input-method-title
    (propertize (format " %s " current-input-method-title)
                'face '(nerd-icons-green minemacs-modeline-inverse-video-face)
                'mouse-face 'mode-line-highlight)))

;;; Buffer status

(minemacs-modeline-define-section buffer-status
  (concat
   (when overwrite-mode
     (concat " " (minemacs-modeline--icon "nf-fa-pencil" :face 'nerd-icons-red)))
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
     (concat " " (minemacs-modeline--icon icon :face face)))))

;;; Dedicated window

(minemacs-modeline-define-section window-dedicated-status
  (when (window-dedicated-p) (concat " " (minemacs-modeline--icon "nf-oct-pin" :face 'nerd-icons-dred))))

;;; Buffer name and modified status

(minemacs-modeline-define-section buffer-identification
  (concat (and buffer-read-only (concat (minemacs-modeline--icon "nf-fa-lock") " "))
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
                        'face 'font-lock-doc-face)))))

(minemacs-modeline-define-section project
  (when-let* ((fname (or (buffer-file-name) default-directory))
              ((not (file-remote-p fname)))
              (proj (project-current))
              (name (project-name proj)))
    (concat
     " "
     (propertize
      (concat " " name " ")
      'face `(,(if (mode-line-window-selected-p) '(mode-line-buffer-id)) minemacs-modeline-inverse-video-face)
      'mouse-face 'mode-line-highlight
      'help-echo (propertize (concat "Project root: " (project-root proj)) 'face 'font-lock-doc-face))
     " ")))

;;; Major mode

(minemacs-modeline-define-section major-mode-icon
  (cond ((buffer-file-name)
         (nerd-icons-icon-for-file (buffer-file-name)))
        ((and (bound-and-true-p dired-mode) dired-directory)
         (nerd-icons-icon-for-dir dired-directory))
        (t (nerd-icons-icon-for-mode major-mode))))

(minemacs-modeline-define-section process
  (list '("" mode-line-process)))

;;; Git branch and diffstat

(declare-function vc-git--symbolic-ref "vc-git" (file))

(defvar minemacs-modeline-vc-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'vc-diff)
    (define-key map [mode-line mouse-3] 'vc-root-diff)
    map))

(defvar minemacs-modeline--vc-faces
  '((added . vc-locally-added-state)
    (edited . vc-edited-state)
    (removed . vc-removed-state)
    (missing . vc-missing-state)
    (conflict . vc-conflict-state)
    (locked . vc-locked-state)
    (up-to-date . vc-up-to-date-state)))

(minemacs-modeline-define-section vc-branch
  (when-let* (((mode-line-window-selected-p))
              (file (or buffer-file-name default-directory))
              ((not (file-remote-p file))) ;; Can be too slow for remote files
              (backend (or (vc-backend file) 'Git))
              (file-state (vc-state file backend))
              (face (alist-get file-state minemacs-modeline--vc-faces 'vc-up-to-date-state))
              (rev (vc-working-revision file backend))
              (branch (or (vc-git--symbolic-ref file) (substring rev 0 7)))
              (help-echo-msg (format "Branch: %s\nRevision: %s\nmouse-1: `vc-diff', diff the current file\nmouse-3: `vc-root-diff', diff all project files" branch rev))
              (branch-trim (if-let* ((len (length branch))
                                     ((> len 15)))
                               (concat "··" (substring branch (- len 15) len))
                             branch)))
    (concat
     (minemacs-modeline--icon "nf-fa-code_branch" :face 'shadow)
     " "
     (propertize branch-trim
                 'face face
                 'mouse-face 'mode-line-highlight
                 'help-echo help-echo-msg
                 'local-map minemacs-modeline-vc-map))))

;;; Flymake errors, warnings, notes

(declare-function flymake--severity "flymake" (type))
(declare-function flymake-diagnostic-type "flymake" (diag))

;; Based on `flymake--mode-line-counter'.
(defun minemacs-modeline-flymake-counter (type)
  "Compute number of diagnostics in buffer with TYPE's severity.
TYPE is usually keyword `:error', `:warning' or `:note'."
  (let ((count 0))
    (dolist (d (flymake-diagnostics))
      (when (= (flymake--severity type) (flymake--severity (flymake-diagnostic-type d)))
        (cl-incf count)))
    (when (cl-plusp count)
      (number-to-string count))))

(defvar minemacs-modeline-flymake-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'flymake-show-buffer-diagnostics)
    (define-key map [mode-line mouse-3] 'flymake-show-project-diagnostics)
    map))

(defmacro minemacs-modeline-flymake-type (type icon &optional face)
  "Return function that handles Flymake TYPE with stylistic ICON and FACE."
  `(defun ,(intern (format "minemacs-modeline-flymake-%s" type)) ()
     (when-let* ((count (minemacs-modeline-flymake-counter ,(intern (format ":%s" type)))))
       (concat
        " "
        (minemacs-modeline--icon ,icon :face ',(or face type))
        " "
        (propertize count
                    'face ',(or face type)
                    'mouse-face 'mode-line-highlight
                    'local-map minemacs-modeline-flymake-map
                    'help-echo "mouse-1: buffer diagnostics\nmouse-3: project diagnostics")))))

(minemacs-modeline-flymake-type error "nf-cod-error" nerd-icons-red)
(minemacs-modeline-flymake-type warning "nf-cod-warning" nerd-icons-orange)
(minemacs-modeline-flymake-type note "nf-cod-info" nerd-icons-green)

(minemacs-modeline-define-section flymake
  (when (and (mode-line-window-selected-p) (bound-and-true-p flymake-mode))
    (list ; See the calls to the macro `minemacs-modeline-flymake-type'
     '(:eval (minemacs-modeline-flymake-error))
     '(:eval (minemacs-modeline-flymake-warning))
     '(:eval (minemacs-modeline-flymake-note)))))

;;; Eglot

(with-eval-after-load 'eglot
  (setq mode-line-misc-info
        (delete '(eglot--managed-mode (" [" eglot--mode-line-format "] ")) mode-line-misc-info)))

(minemacs-modeline-define-section eglot
  (when (and (mode-line-window-selected-p) (featurep 'eglot))
    '(eglot--managed-mode eglot--mode-line-format)))

;;; Compilation

(minemacs-modeline-define-section compile
  (when (and (mode-line-window-selected-p) (bound-and-true-p compilation-in-progress))
    (propertize (minemacs-modeline--icon "nf-fa-hammer" :face 'nerd-icons-red)
                'mouse-face 'mode-line-highlight
                'help-echo (mapconcat #'buffer-name (mapcar #'process-buffer compilation-in-progress) "\n"))))

;;; `dired-rsync'

(defvar minemacs-modeline-dired-rsync-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] (lambda () (interactive)
                                               (when-let* ((proc (get-process "*rsync*")))
                                                 (pop-to-buffer (process-buffer proc)))))
    map))

(minemacs-modeline-define-section dired-rsync
  (when-let* ((proc (get-process "*rsync*")))
    (propertize (minemacs-modeline--icon "nf-oct-sync" :face 'nerd-icons-blue)
                'mouse-face 'mode-line-highlight
                'help-echo (buffer-name (process-buffer proc))
                'local-map minemacs-modeline-dired-rsync-map)))

;;; Miscellaneous

(minemacs-modeline-define-section misc-info
  (when (mode-line-window-selected-p) mode-line-misc-info))

;;; Mode

(defvar minemacs-modeline--mode-line-format-orig nil)

;;;###autoload
(define-minor-mode minemacs-modeline-mode
  "MinEmacs' mode-line."
  :global t
  (if minemacs-modeline-mode
      (progn
        (unless minemacs-modeline--mode-line-format-orig
          (setq minemacs-modeline--mode-line-format-orig (default-value 'mode-line-format)))
        (setq-default mode-line-format
                      '("%e"
                        minemacs-modeline-multiple-cursors
                        minemacs-modeline-kbd-macro
                        minemacs-modeline-narrow
                        minemacs-modeline-buffer-status
                        minemacs-modeline-window-dedicated-status
                        minemacs-modeline-input-method
                        "  "
                        minemacs-modeline-project
                        minemacs-modeline-major-mode-icon
                        " "
                        minemacs-modeline-buffer-identification
                        "  "
                        mode-line-position
                        "  "
                        minemacs-modeline-process
                        "  "
                        minemacs-modeline-eglot
                        "  "
                        mode-line-format-right-align
                        "  "
                        minemacs-modeline-compile
                        "  "
                        minemacs-modeline-dired-rsync
                        "  "
                        minemacs-modeline-vc-branch
                        "  "
                        minemacs-modeline-flymake
                        "  "
                        minemacs-modeline-misc-info
                        "  "))
        (+subtle-mode-line)
        (add-hook 'server-after-make-frame-hook #'+subtle-mode-line)
        (add-hook 'enable-theme-functions #'+subtle-mode-line))
    (setq-default mode-line-format minemacs-modeline--mode-line-format-orig)
    (remove-hook 'server-after-make-frame-hook #'+subtle-mode-line)
    (remove-hook 'enable-theme-functions #'+subtle-mode-line))
  (force-mode-line-update t))


(provide 'minemacs-modeline)
;;; minemacs-modeline.el ends here
