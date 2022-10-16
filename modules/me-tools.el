;; -*- lexical-binding: t; -*-


(use-package rg
  :straight t
  :defer t
  :general
  (me-map "/" '(rg :which-key "ripgrep")))


(use-package affe
  :straight t
  :after consult orderless
  :general
  (me-map
    "sg" #'affe-grep
    "sf" #'affe-find)
  :config
  ;; Use orderless to compile regexps
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (orderless-pattern-compiler input))
    (cons input (lambda (str) (orderless--highlight input str))))

  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler)

  ;; Manual preview keys
  (consult-customize affe-grep :preview-key (kbd "M-p"))
  (consult-customize affe-find :preview-key (kbd "M-p")))


(use-package tldr
  :straight t
  :commands (tldr-update-docs tldr)
  :custom
  (tldr-enabled-categories '("common" "linux" "osx"))
  (tldr-directory-path (expand-file-name "tldr" minemacs-local-dir)))


(use-package vterm
  :straight t
  :general
  (me-map
    "ot" '(+popwin:vterm :which-key "vTerm popup")
    "oT" '(vterm :which-key "vTerm"))
  :preface
  (when noninteractive
    (advice-add #'vterm-module-compile :override #'ignore)
    (provide 'vterm-module))
  :custom
  (vterm-max-scrollback 5000)
  :config
  (with-eval-after-load 'popwin
    (defun +popwin:vterm ()
      (interactive)
      (popwin:display-buffer-1
       (or (get-buffer "*vterm*")
           (save-window-excursion
             (call-interactively 'vterm)))
       :default-config-keywords '(:position :bottom :height 12)))))


(with-eval-after-load 'popwin
  (me-map
    "oe" '(+popwin:eshell :which-key "Eshell popup")
    "oE" '(eshell :which-key "Eshell"))
  (defun +popwin:eshell ()
    (interactive)
    (popwin:display-buffer-1
     (or (get-buffer "*eshell*")
         (save-window-excursion
           (call-interactively 'eshell)))
     :default-config-keywords '(:position :bottom :height 12))))


(provide 'me-tools)
