;;; me-cape-super-capf.el --- Combine multiple capes -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:
;; Make use of `cape''s super Capf functionality. Taken from:
;; https://git.sr.ht/~gagbo/doom-config/tree/master/item/modules/completion/corfu/config.el

;;; Code:

(defgroup minemacs-completion nil
  "Completion related stuff."
  :group 'minemacs)

;;;###autoload
(defcustom +cape-global-capes
  '(tempel-complete :completion cape-dict)
  "A list of global capes to be available at all times.
The key :completion is used to specify where completion candidates should be
placed, otherwise they come first."
  :group 'minemacs-completion
  :type '(repeat symbol))

;;;###autoload
(defcustom +cape-hosts
  '(eglot-completion-at-point
    lsp-completion-at-point
    elisp-completion-at-point
    tags-completion-at-point-function)
  "A prioritised list of host capfs to create a super cape onto from
`+cape-global-capes'."
  :group 'minemacs-completion
  :type '(repeat symbol))

;;;###autoload
(defun +cape-load-capes ()
  "Load all capes specified in `+cape-global-capes'."
  (interactive)
  (when-let ((host (cl-intersection +cape-hosts completion-at-point-functions)))
    (setq-local
     completion-at-point-functions
     (cl-substitute
      (apply
       #'cape-super-capf
       (cl-substitute
        (car host)
        :completion
        (append (cl-pushnew :completion +cape-global-capes))))
      (car host)
      completion-at-point-functions))))

;;;###autoload
(defun +toggle-cape-auto-super-capf (&optional disable)
  "Enable auto generating Cape's super Capf.
This depends on `+cape-hosts' and `+cape-global-capes'."
  (interactive)
  (let ((enabled (get '+cape-auto-super-capf 'enabled)))
    (dolist (hook '(lsp-mode-hook eglot-managed-mode-hook change-major-mode-hook))
      (apply (if (or enabled disable) #'remove-hook #'add-hook) (list hook #'+cape-load-capes))
      (put '+cape-auto-super-capf 'enabled (not (or enabled disable))))))


(provide 'me-cape-super-capf)

;;; me-cape-super-capf.el ends here
