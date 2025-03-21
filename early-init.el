;;; early-init.el --- MinEmacs early initialization tweaks -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(dolist (dir '("core" "modules" "modules/extras" "elisp")) ; Add some of MinEmacs' directories to `load-path'
  (add-to-list 'load-path (expand-file-name dir (file-name-directory (file-truename load-file-name)))))

(require 'me-lib)

(setq
 ;; Do not make installed packages available when Emacs starts (we use `straight')
 package-enable-at-startup nil
 ;; Better garbage collection settings, no GCMH required, See: https://zenodo.org/records/10518083
 gc-cons-threshold (* 100 1000 1000)
 gc-cons-percentage 0.2
 ;; Prefer loading newer files
 load-prefer-newer t
 ;; Remove some unneeded UI elements
 default-frame-alist '((tool-bar-lines . 0)
                       (menu-bar-lines . 0)
                       (vertical-scroll-bars)
                       (left-fringe . 8)
                       (right-fringe . 13)
                       (internal-border-width . 15)
                       (fullscreen . maximized))
 ;; Explicitly set modes disabled in `default-frame-alist' to nil
 tool-bar-mode nil
 menu-bar-mode nil
 scroll-bar-mode nil
 ;; Set mode-line format to prevent it from showing at startup
 mode-line-format nil)

;; It seems like, even when `tool-bar-mode' is nil, `tool-bar-setup' still be called
(advice-add 'tool-bar-setup :override #'ignore)

;; PERF: Setting `file-name-handler-alist' to nil should boost startup time.
;; https://reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start
;; Store the current value so we can reset it after Emacs startup.
(put 'file-name-handler-alist 'original-value (default-toplevel-value 'file-name-handler-alist))
(set-default-toplevel-value 'file-name-handler-alist nil) ; Make sure the new value survives any current let-binding
;; Restore `file-name-handler-alist' after startup while conserving the potential new elements
(add-hook 'emacs-startup-hook (lambda () (setq file-name-handler-alist (delete-dups (append file-name-handler-alist (get 'file-name-handler-alist 'original-value))))) 99)

;; Load the user early configuration files
(+load-user-configs 'early-config 'local/early-config)

(provide 'early-init)
;;; early-init.el ends here
