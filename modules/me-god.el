;;  me-god.el -- God mode -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(use-package god-mode
  :straight t
  :bind (("M-<escape>" . god-local-mode))
  :custom
  (god-mode-enable-function-key-translation nil)
  :config
  ;; Change cursor shape, a box when in `god-mode' and a bar when not
  (+setq-hook! god-mode-enabled cursor-type 'box)
  (+setq-hook! god-mode-disabled cursor-type 'bar)

  ;; Pause `god-mode' when entering `overwrite-mode'
  (add-hook
   'overwrite-mode-hook
   (satch-defun +god-mode--toggle-on-overwrite-h ()
     (if overwrite-mode (god-local-mode-pause) (god-local-mode-resume))))

  ;; Enable `which-key' integration for `god-mode' (EXPERIMENTAL)
  (with-eval-after-load 'which-key
    (which-key-enable-god-mode-support))

  ;; Integration with `isearch' --> TODO: add some visual indication for it
  (with-eval-after-load 'isearch
    (require 'god-mode-isearch)
    (keymap-set-after isearch-mode-map "<escape>" #'god-mode-isearch-activate 'isearch)
    (keymap-set-after god-mode-isearch-map "<escape>" #'god-mode-isearch-disable 'isearch))

  ;; Complexify some commands to be simplified by `god-mode', `x 1', `x 0' and so on
  (keymap-global-set "C-x C-1" #'delete-other-windows)
  (keymap-global-set "C-x C-2" #'split-window-below)
  (keymap-global-set "C-x C-3" #'split-window-right)
  (keymap-global-set "C-x C-0" #'delete-window)

  ;; Use "i" to turn off `god-mode' and "." to repeat last command
  (keymap-set god-local-mode-map "i" #'god-local-mode)
  (keymap-set god-local-mode-map "." #'repeat)
  (keymap-set god-local-mode-map "(" #'backward-paragraph)
  (keymap-set god-local-mode-map ")" #'forward-paragraph))


(provide 'me-god)
;;; me-god.el ends here
