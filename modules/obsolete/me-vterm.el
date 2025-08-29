;;; me-vterm.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-08-29
;; Last modified: 2025-08-29

;;; Commentary:

;;; Code:


;; Fully-fledged terminal emulator inside Emacs based on "libvterm"
(use-package vterm
  :straight t
  :when (and (not (featurep 'os/win)) (featurep 'feat/modules))
  :hook
  (minemacs-build-functions . vterm-module-compile)
  (vterm-mode . compilation-shell-minor-mode)
  (vterm-mode . minemacs-reduce-font-size)
  :bind (:map vterm-mode-map ([return] . vterm-send-return))
  :init
  (+def-dedicated-tab! vterm :exit-hook vterm-exit-functions)
  :custom
  (vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=Off")
  (vterm-always-compile-module t)
  (vterm-max-scrollback 5000)
  :config
  (add-to-list 'display-buffer-alist `("\\*Install vterm\\*" (display-buffer-no-window) (allow-no-window . t))))


;; Managing multiple vterm buffers in Emacs
(use-package multi-vterm
  :straight t
  :when (and (not (featurep 'os/win)) (featurep 'feat/modules))
  :bind (([remap project-shell] . multi-vterm-project)
         ([f1] . +multi-vterm-toggle-dwim)
         :map vterm-mode-map ([f1] . +multi-vterm-toggle-dwim))
  :custom
  (multi-vterm-dedicated-window-height-percent 20)
  :config
  ;; If a dedicated terminal is run on a remote machine, it seems that
  ;; `multi-vterm' don't get the working directory right, lets fix it!
  (advice-add
   'multi-vterm-dedicated-open :after
   (satch-defun +multi-vterm--remote-change-working-directory:after-a (&rest _)
     (when-let* ((dir (file-remote-p default-directory 'localname)))
       (vterm-send-string (format "cd " (shell-quote-argument dir)))
       (vterm-send-return))))

  (defun +multi-vterm-toggle-dwim ()
    "Toggle the vterm window.
When in a project, toggle a `multi-vterm-project' terminal. When outside
a project, call `multi-vterm-dedicated-toggle'."
    (interactive)
    (if-let* ((buf-name (and (multi-vterm-project-root) (multi-vterm-project-get-buffer-name))))
        (if-let* ((buf (get-buffer buf-name))
                  ((buffer-live-p buf)))
            (if-let* ((win (get-buffer-window buf))) ; The project's vterm already exists, toggle it's window
                (delete-window win)
              (pop-to-buffer buf))
          (multi-vterm-project))
      (multi-vterm-dedicated-toggle))))


(provide 'obsolete/me-vterm)
;;; me-vterm.el ends here
