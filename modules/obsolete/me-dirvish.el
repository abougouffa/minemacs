;;; me-dirvish.el --- Dirvish integration -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;; Causing some errors (detected on Emacs 30.0.60), printing contineously to the messages buffer
;;
;; Error during redisplay: (eval (dirvish-sort-ml dv) t) signaled (void-variable dv)
;; Error during redisplay: (eval (dirvish-file-time-ml dv) t) signaled (void-variable dv)
;; Error during redisplay: (eval (dirvish-symlink-ml dv) t) signaled (void-variable dv)
;; Error during redisplay: (eval (dirvish-omit-ml dv) t) signaled (void-variable dv)
;; Error during redisplay: (eval (dirvish-yank-ml dv) t) signaled (void-variable dv)
;; Error during redisplay: (eval (dirvish-index-ml dv) t) signaled (void-variable dv)
;; Error during redisplay: (eval (dirvish-path-ml dv) t) signaled (void-variable dv)

;;; Code:

(use-package dirvish
  :straight t
  :after dired
  :demand
  :custom
  (dirvish-attributes '(subtree-state nerd-icons file-size))
  (dirvish-mode-line-format '(:left (sort file-time symlink) :right (omit yank index)))
  (dirvish-side-width 30)
  (dirvish-fd-default-dir "~/")
  (dirvish-use-header-line t) ; 'global make header line span all panes
  (dirvish-use-mode-line t)
  (dirvish-subtree-state-style 'nerd)
  :config
  ;; Cscope generate *.po files which that makes dirvish preview freeze
  (push "po" dirvish-preview-disabled-exts)
  ;; Use `nerd-icons' for path separators (from https://github.com/rainstormstudio/nerd-icons.el)
  (with-eval-after-load 'nerd-icons
    (setq dirvish-path-separators (list (format "  %s " (nerd-icons-codicon "nf-cod-home"))
                                        (format "  %s " (nerd-icons-codicon "nf-cod-root_folder"))
                                        (format " %s " (nerd-icons-faicon "nf-fa-angle_right")))))
  (dirvish-override-dired-mode 1))


(provide 'obsolete/me-dirvish)
;;; me-dirvish.el ends here
