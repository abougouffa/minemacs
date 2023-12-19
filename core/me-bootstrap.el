;; me-bootstrap.el --- Bootstrap packages (elpaca & use-package) -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" minemacs-local-dir))
(defvar elpaca-builds-directory (expand-file-name (format "builds-%s%s/" emacs-version (if emacs-repository-version (format "-%s" (substring emacs-repository-version 0 8)) "")) elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca
                          :repo "https://github.com/progfolio/elpaca.git"
                          :ref nil
                          :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                          :build (:not elpaca--activate-package)))
(let* ((repo (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone" (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout" (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch" "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))

(add-hook 'after-init-hook #'elpaca-process-queues)

(elpaca `(,@elpaca-order))

;; Install `use-package' if not available (Emacs 28)
(unless (require 'use-package nil t)
  (elpaca use-package))

;; Block until current queue processed.
(elpaca-wait)

;; Install `elpaca' integration with `use-package'
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword
  (elpaca-use-package-mode 1))

;; Add support for `minemacs-disabled-packages' and better conditional blocks.
(require 'me-use-package-extra)

;; Block until current queue processed.
(elpaca-wait)

(setq
 ;; Set `use-package' to verbose when MinEmacs is started in verbose mode
 use-package-verbose (cond (minemacs-debug-p 'debug) (minemacs-verbose-p t))
 ;; Defer loading packages by default, use `:demand' to force loading a package
 use-package-always-defer (not minemacs-always-demand-p)
 use-package-always-demand minemacs-always-demand-p
 ;; Make the expanded code as minimal as possible, do not try to catch errors
 use-package-expand-minimally (not minemacs-debug-p))


(provide 'me-bootstrap)

;;; me-bootstrap.el ends here
