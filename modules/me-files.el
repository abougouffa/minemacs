;; -*- lexical-binding: t; -*-


(use-package dirvish
  :straight t
  :after minemacs-loaded
  :general
  (me-map
    ;; Open
    "o-"  '(dirvish :which-key "Dirvish")
    "op"  '(dirvish-side :which-key "Side panel")
    "oq"  '(dirvish-quick-access :which-key "Dirvish quick access")
    ;; Search
    "sd"  '(dirvish-fd :which-key "Dirvish fd"))
  :config
  (require 'dirvish-icons)
  (setq dirvish-cache-dir (expand-file-name "dirvish/" minemacs-cache-dir)
        dirvish-fd-default-dir (expand-file-name "~")
        dirvish-mode-line-format
        '(:left (sort file-time symlink) :right (omit yank index)))
  ;; Set attributes
  (setq dirvish-attributes
        (delete-dups
         (append
          dirvish-attributes
          '(vc-state git-msg all-the-icons))))
  (dirvish-override-dired-mode))


(provide 'me-files)
