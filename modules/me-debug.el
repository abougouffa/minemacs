;;; me-debug.el --- Debugging stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package gdb-mi
  :straight (:type built-in)
  :custom
  (gdb-show-main t) ; display source file containing main routine at startup
  (gdb-many-windows t) ; start in gdb-many-windows mode
  (gdb-debug-log-max 1024) ; default 128
  (gdb-restore-window-configuration-after-quit t)
  (gdb-thread-buffer-verbose-names nil)
  (gdb-window-configuration-directory (+directory-ensure minemacs-local-dir "gdb/"))
  (gdb-max-source-window-count 1) ; IDEA: maybe increase it!
  (gdb-display-io-nopopup nil) ; IDEA: maybe change it!
  :config
  ;; Add an overlay for the current line (mimics dap-mode)
  (defvar +gud-overlay
    (let* ((overlay (make-overlay (point-min) (point-min))))
      (overlay-put overlay 'face 'highlight)
      overlay)
    "Overlay variable for GUD highlighting.")

  (advice-add
   'gud-display-line :after
   (defun +gud--display-overlay-a (true-file _line)
     (let* ((overlay +gud-overlay)
            (buffer (gud-find-file true-file)))
       (with-current-buffer buffer
         (move-overlay overlay (line-beginning-position) (line-end-position) (current-buffer))))))

  (add-hook
   'kill-buffer-hook
   (defun +gud--delete-overlay-h ()
     (when (derived-mode-p 'gud-mode)
       (delete-overlay +gud-overlay)))))

(use-package realgud
  :straight t
  :init
  (+map-local! :keymaps '(c-mode-map c++-mode-map python-mode-map
                          rust-mode-map rust-ts-mode-map
                          sh-mode-map bash-ts-mode-map)
    "r" '(nil :wk "realgud")
    "rd" #'+realgud:start
    "rh" #'+realgud-hydra/body))

(use-package realgud-lldb
  :straight t
  :commands realgud--lldb)

(use-package realgud-ipdb
  :straight t)

(use-package disaster
  :straight t
  :preface
  (defconst +objdump-available-p (executable-find "objdump"))
  :when +objdump-available-p
  :init
  (+map-local! :keymaps '(c-mode-map c++-mode-map fortran-mode-map)
    "D" #'disaster))


(provide 'me-debug)

;;; me-debug.el ends here
