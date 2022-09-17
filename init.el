;;; init.el --- MinEmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <hacko@laptop>

;; Profile emacs startup
(add-hook
 'emacs-startup-hook
 (lambda ()
   (message "Emacs loaded in %s." (emacs-init-time))))

(defconst IS-LINUX (memq system-type '(gnu gnu/linux)))
(defconst IS-BSD   (memq system-type '(darwin berkeley-unix)))
(defconst IS-WIN   (memq system-type '(cygwin windwos-nt ms-dos)))

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

(require 'cl-lib)

(dolist (module '("bootstrap.el"
                  "defaults.el"
                  "utils.el"
                  "keybindings.el"
                  "ui.el"
                  "editor.el"
                  "evil.el"
                  "prog.el"
                  "lisps.el"
                  "completion.el"))
  (load module))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((org-export-in-background)
     (geiser-scheme-implementation quote guile)
     (eval progn
	   (setq org-latex-listings 'minted)
	   (setq org-latex-minted-options
		 '(("frame" "lines")
		   ("fontsize" "\\footnotesize")
		   ("tabsize" "2")
		   ("breaklines" "true")
		   ("breakanywhere" "true")
		   ("style" "default")
		   ("bgcolor" "MyBGGray")
		   ("linenos" "true")))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:font "Fira Code 15"))))
 '(fixed-pitch ((t (:inherit (default)))))
 '(fixed-pitch-serif ((t (:inherit (default)))))
 '(variable-pitch ((t (:font "Fira Code 15")))))
