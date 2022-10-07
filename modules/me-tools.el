;; -*- lexical-binding: t; -*-


(use-package rg
  :straight t
  :defer t
  :general
  (me-map "/" '(rg :which-key "ripgrep")))


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
  :config
  (setq vterm-kill-buffer-on-exit t
        vterm-max-scrollback 5000)

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
