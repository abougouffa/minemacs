;;; me-gdb.el --- Extra tweaks for GDB, and opt-in emacs-gdb integration -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(require 'gdb-mi)

(defvar +gdb--old-win-config nil)
(defvar +gdb--many-windows-old nil)

;; From stackoverflow.com/q/39762833/846686
(defun +gdb--set-layout (&optional c-buffer)
  "Set the GDB layout around the current buffer C-BUFFER."
  (set-window-dedicated-p (selected-window) nil) ;; unset dedicate state if needed
  (switch-to-buffer gud-comint-buffer)
  (delete-other-windows) ;; clean all

  (let* ((c-buffer (or c-buffer (window-buffer (selected-window)))) ;; save original buffer
         (w-source (selected-window)) ;; left top
         (w-gdb (split-window w-source nil 'right)) ;; right bottom
         (w-locals (split-window w-gdb nil 'above)) ;; right middle bottom
         (w-stack (split-window w-locals nil 'above)) ;; right middle top
         (w-breakpoints (split-window w-stack nil 'above)) ;; right top
         (w-io (split-window w-source (floor(* 0.9 (window-body-height))) 'below))) ;; left bottom
    (set-window-buffer w-io (gdb-get-buffer-create 'gdb-inferior-io))
    (set-window-dedicated-p w-io t)
    (set-window-buffer w-breakpoints (gdb-get-buffer-create 'gdb-breakpoints-buffer))
    (set-window-dedicated-p w-breakpoints t)
    (set-window-buffer w-locals (gdb-get-buffer-create 'gdb-locals-buffer))
    (set-window-dedicated-p w-locals t)
    (set-window-buffer w-stack (gdb-get-buffer-create 'gdb-stack-buffer))
    (set-window-dedicated-p w-stack t)

    (set-window-buffer w-gdb gud-comint-buffer)

    (select-window w-source)
    (set-window-buffer w-source c-buffer)))

;;;###autoload
(defun +gdb-set-layout ()
  "Enable custom window layout for gdb."
  (interactive)
  (setq +gdb--many-windows-old gdb-many-windows
        gdb-many-windows nil)

  (advice-add
   'gdb :around
   (satch-defun +gdb--set-window-layout:around-a (origfn &rest args)
     ;; Save current buffer
     (setq +gdb--old-win-config (current-window-configuration))
     (let ((c-buffer (window-buffer (selected-window))))
       (apply origfn args)
       (+gdb--set-layout c-buffer))))

  (advice-add
   'gdb-reset :after
   (satch-defun +gdb--restore-window-layout:after-a (&rest _)
     (when +gdb--old-win-config
       (set-window-configuration +gdb--old-win-config)))))

(defun +gdb-reset-layout ()
  "Enable custom window layout for gdb."
  (interactive)
  (setq gdb-many-windows +gdb--many-windows-old)
  (advice-remove 'gdb #'+gdb--set-window-layout:around-a)
  (advice-remove 'gdb-reset #'+gdb--restore-window-layout:after-a))

;;;###autoload
(defun +emacs-gdb-enable ()
  "Load a faster \"gdb\" command from \"emacs-gdb\".
This will overwrite the built-in \"gdb-mi\" for this session."
  (interactive)
  (if (+emacs-features-p 'modules)
      (when (y-or-n-p "Loading \"emacs-gdb\" will overwrite \"gdb-mi\" for this session, continue?")
        (use-package gdb-mi
          ;; I use my own fork in which I've merged some open PRs on the upstream.
          :straight (:host github :repo "abougouffa/emacs-gdb" :files (:defaults "*.c" "*.h" "Makefile"))
          :demand
          :init
          (fmakunbound 'gdb)
          (fmakunbound 'gdb-enable-debug)
          :custom
          (gdb-window-setup-function #'gdb--setup-windows)
          (gdb-ignore-gdbinit nil)))
    (user-error "Cannot enable \"emacs-gdb\", Emacs was built without modules support!")))


(provide 'me-gdb)

;;; me-gdb.el ends here
