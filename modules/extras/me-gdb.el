;;; me-gdb.el --- Extra tweaks for GDB, and opt-in emacs-gdb integration -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

(defun +gdb--set-layout (&optional c-buffer)
  ;; from stackoverflow.com/q/39762833/846686
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
  (require 'gdb-mi)

  (setq +gdb--many-windows-old gdb-many-windows
        gdb-many-windows nil)

  (advice-add
   'gdb :around
   (defun +gdb--set-window-layout-a (origfn &rest args)
     ;; Save current buffer
     (setq +gdb--old-win-config (current-window-configuration))
     (let ((c-buffer (window-buffer (selected-window))))
       (apply origfn args)
       (+gdb--set-layout c-buffer))))

  (advice-add
   'gdb-reset :after
   (defun +gdb--restore-window-layout-a (&rest _)
     (set-window-configuration +gdb--old-win-config))))

(defun +gdb-reset-layout ()
  "Enable custom window layout for gdb."
  (interactive)
  (setq gdb-many-windows +gdb--many-windows-old)
  (advice-remove 'gdb #'+gdb--set-window-layout-a)
  (advice-add 'gdb-reset #'+gdb--restore-window-layout-a))

;;;###autoload
(defun +emacs-gdb-enable ()
  "Load a faster \"gdb\" command from \"emacs-gdb\".
This will overrite the built-in \"gdb-mi\" for this session."
  (interactive)
  (when (yes-or-no-p "Loading \"emacs-gdb\" will overrite \"gdb-mi\" for this session, continue?")
    (use-package gdb-mi
      :straight (:host github :repo "weirdNox/emacs-gdb" :files (:defaults "*.c" "*.h" "Makefile"))
      :when (+emacs-features-p 'modules)
      :demand t
      :init
      (fmakunbound 'gdb)
      (fmakunbound 'gdb-enable-debug)
      :custom
      (gdb-window-setup-function #'gdb--setup-windows)
      (gdb-ignore-gdbinit nil))))
