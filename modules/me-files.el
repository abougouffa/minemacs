;; -*- lexical-binding: t; -*-


(use-package dirvish
  :straight t
  :after minemacs-loaded
  :custom
  (dirvish-attributes '(file-size vc-state git-msg all-the-icons))
  (dirvish-cache-dir (expand-file-name "dirvish/" minemacs-cache-dir))
  (dirvish-mode-line-format '(:left (sort file-time symlink) :right (omit yank index)))
  (dirvish-side-width 30)
  :config
  (+map
    ;; Open
    "o-"  '(dirvish :which-key "Dirvish")
    "op"  '(dirvish-side :which-key "Side panel")
    "oq"  '(dirvish-quick-access :which-key "Dirvish quick access")
    ;; Search
    "sd"  '(dirvish-fd :which-key "Dirvish fd"))
  (+map-key :keymaps 'dirvish-mode-map
    "q" #'dirvish-quit)
  (dirvish-override-dired-mode))


(use-package dirvish-fd
  :after dirvish
  :custom
  (dirvish-fd-default-dir (expand-file-name "~/")))


(use-package dirvish-yank
  :after dirvish)


(use-package dirvish-icons
  :after dirvish)


(use-package dirvish-emerge
  :after dirvish)


(provide 'me-files)
