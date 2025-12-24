;; me-bootstrap.el --- Bootstrap packages (straight & use-package) -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-09-17
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

(require 'me-lib)
(require 'use-package)



;; HACK: This advice around `use-package' checks if a package is disabled in
;; `minemacs-disabled-packages' before calling `use-package'. This can come
;; handy if the user wants to enable some module while excluding some packages
;; from it. This advice also evaluates `use-package's conditional sections
;; (`:if', `:when' and `:unless') to prevent installing packages with
;; `straight'.
(defun +use-package--check-if-disabled:around-a (origfn package &rest args)
  (if (and +use-package-check-for-disabled
           (or (+package-disabled-p package)
               (memq :disabled args)
               (and (memq :if args) (not (eval (+varplist-get args :if t))))
               (and (memq :when args) (not (eval (+varplist-get args :when t))))
               (and (memq :unless args) (eval (+varplist-get args :unless t)))))
      ;; Register the package but don't enable it, useful when creating the lockfile,
      ;; this is the official straight.el way for conditionally installing packages
      (when-let* ((recipe (+varplist-get args :ensure t)))
        (let* ((recipe (if (eq recipe t) (list package) recipe))
               (car-recipe (and (listp recipe) (car recipe)))
               (car-recipe-is-pkg (and (symbolp car-recipe) (not (keywordp car-recipe))))
               (recipe (if (and car-recipe car-recipe-is-pkg) recipe (append (list package) recipe))))
          (when (fboundp 'straight-register-package)
            (straight-register-package recipe))))
    ;; Otherwise, add it to the list of configured packages and apply the `use-package' form
    (add-to-list 'minemacs-configured-packages package t)
    (apply origfn package args)))

(advice-add 'use-package :around '+use-package--check-if-disabled:around-a)

(defun minemacs-bump-packages ()
  "Update MinEmacs packages to the last revisions (can cause breakages)."
  (interactive)
  (user-error "Not implemented yet")
  (minemacs-run-build-functions 'dont-ask)) ; Run package-specific build functions (ex: `pdf-tools-install')

(defun minemacs-bump-packages-async ()
  "Like `minemacs-bump-packages', but runs asynchronously."
  (interactive)
  (let* ((compilation-buffer-name-function (lambda (&rest _args) "*minemacs-bump-packages*")))
    (compile (concat (car command-line-args) " --batch --eval='(minemacs-bump-packages)' --script " user-init-file))))

(defun minemacs-upgrade (pull)
  "Upgrade the packages list to the locked revisions.
This takes into account the explicitly pinned packages. When called with
\\[universal-argument] or with PULL, it will run \"git pull\" in
MinEmacs directory before upgrading."
  (interactive "P")
  (when pull (let ((default-directory minemacs-root-dir)) (vc-pull)))
  (user-error "Not implemented yet")
  (minemacs-run-build-functions 'dont-ask)) ; Run package-specific build functions (ex: `pdf-tools-install')


(provide 'me-bootstrap)

;;; me-bootstrap.el ends here
