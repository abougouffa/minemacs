;; -*- lexical-binding: t; -*-


;; Icons
(use-package all-the-icons
  :straight t
  :defer t
  :config
  ;; Show .m files as matlab/octave files
  (setcdr (assoc "m" all-the-icons-extension-icon-alist)
          (cdr (assoc "matlab" all-the-icons-extension-icon-alist))))


;; Themes
(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-one-light t))


;; Modeline
(use-package doom-modeline
  :straight t
  :config
  (setq doom-modeline-bar-width 6
        doom-modeline-height 32)

  (doom-modeline-def-segment time
    (when (and doom-modeline-time
               (bound-and-true-p display-time-mode)
               (not doom-modeline--limited-width-p))
      (concat
       doom-modeline-spc
       (when doom-modeline-time-icon
         (concat
          (doom-modeline-icon 'faicon "clock-o" "🕘" ""
                              :face 'mode-line
                              :v-adjust -0.05)
          (and (or doom-modeline-icon doom-modeline-unicode-fallback)
               doom-modeline-spc)))
       (propertize display-time-string
                   'face (doom-modeline-face 'doom-modeline-time)))))

  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number modals matches follow buffer-info
      remote-host buffer-position word-count parrot selection-info)
    '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug
      repl lsp minor-modes input-method indent-info buffer-encoding major-mode
      process vcs checker time "   "))

  (doom-modeline-mode 1))


(provide 'me-core-ui)
