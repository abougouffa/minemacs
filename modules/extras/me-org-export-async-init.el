;; -*- lexical-binding: t; -*-

;; This file will be used as `user-init-file' when exporting Org documents
;; asynchronously. This will set the modules list to the minimal required to
;; export Org documents.

;; BUG For some reason, exporting in background can fail with this error: (error
;; "Odd length text property list"), in such case, you can remove the Org cache
;; directory and retry again (rm -rf ~/.emacs.d/local/cache/org/)
;; (https://github.com/org-roam/org-roam/issues/2155#issuecomment-1145388814)

(message "Using MinEmacs' \"me-org-export-async-init.el\" as init file.")

;; This signals that we are running in a org-export-async context
(provide 'me-org-export-async-init)

;; Load only some essential modules
(defvar minemacs-core-modules
  '(me-defaults me-bootstrap me-keybindings))

(defvar minemacs-modules
  '(me-org me-biblio me-latex me-prog me-lisp))

(load (concat user-emacs-directory "init.el") nil t)

(message "Loaded %d modules!" (+ (length minemacs-core-modules) (length minemacs-modules)))
