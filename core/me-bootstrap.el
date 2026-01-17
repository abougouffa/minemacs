;; me-bootstrap.el --- Bootstrap packages (straight & use-package) -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-09-17
;; Last modified: 2026-01-18

;;; Commentary:

;;; Code:

(require 'me-lib)
(require 'use-package)



;;; Tweak `straight' parameters

(setq
 ;; Base directory
 straight-base-dir minemacs-local-dir
 ;; Add Emacs version and the Git hash to the build directory to avoid problems
 straight-build-dir (format "build-%s%s" emacs-version (if emacs-repository-version (format "-%s" (substring emacs-repository-version 0 8)) ""))
 ;; Use the "develop" branch on straight.el's repo
 straight-repository-branch "develop"
 ;; Do not slow startup by checking for package modifs, check only on demand
 straight-check-for-modifications '(check-on-save find-when-checking))



;;; Bootstrap `straight'
;; See: https://github.com/radian-software/straight.el#bootstrapping-straightel

(defvar bootstrap-version)
(let ((bootstrap-file (concat straight-base-dir "straight/repos/straight.el/bootstrap.el"))
      (install-url "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el")
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer (url-retrieve-synchronously install-url 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))



;;; `straight' customization

;; For `straight' to not install built-in packages from ELPA
(cl-callf append straight-built-in-pseudo-packages
  '(treesit org tramp project editorconfig flymake eldoc eglot xref imenu modus-themes))

;; HACK+PERF: Reduce installation time and disk usage using "--filter=tree:0".
;; This cuts the size of the "straight/repos" directory by more than half (from
;; 807M to 362M) while keeping it possible to checkout and download older
;; commits on-demand (unlike "--depth=N"). The parameter is injected when a "git
;; clone" command call is requested in `straight--process-run' which is called
;; from `straight-vc-git--clone-internal'
(advice-add
 'straight--process-run :filter-args
 (lambda (args)
   (if (and (equal (car args) "git") (equal (cadr args) "clone"))
       `("git" "clone" "--filter=tree:0" ,@(cddr args))
     args)))

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
      (when-let* ((recipe (+varplist-get args :straight t)))
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

;; Do some tweaks to avoid asking questions when running
;; `minemacs-bump-packages'
(defun +minemacs--straight--popup-raw:around-a (orig-fn prompt actions)
  (let (action-func action-args)
    (dolist (action actions)
      (let ((desc (nth 1 action))
            (func (nth 2 action))
            (args (cdddr action)))
        (when (seq-some (lambda (str) (string-match-p str desc))
                        '("Stash changes" "Reset .* to .*" "Checkout .*"
                          "Skip this repository for now and come back to it later"
                          "Rename remote .* to .*, re-create .* with correct URL, and fetch"))
          (setq action-func func
                action-args args))))
    (if action-func
        (apply action-func action-args)
      (funcall orig-fn prompt actions))))

(defun +minemacs--read-string:around-a (orig-fn prompt &rest args)
  (cond
   ((string-match-p "Optional stash message:" prompt)
    (format-time-string "Stashed when bumping the package %F at %T"))
   (t (apply orig-fn (cons prompt args)))))

(defun +minemacs--y-or-n-p:around-a (orig-fn prompt)
  (cond
   ((equal prompt "Caches are outdated, reload init-file? ")
    nil)
   (t (funcall orig-fn prompt))))

(defmacro +straight-with-no-questions (&rest body)
  "Execute BODY and don't ask `straight' questions."
  (declare (indent 0))
  `(unwind-protect
       (progn
         (advice-add 'straight--popup-raw :around '+minemacs--straight--popup-raw:around-a)
         (advice-add 'read-string :around '+minemacs--read-string:around-a)
         (advice-add 'y-or-n-p :around '+minemacs--y-or-n-p:around-a)
         ,(macroexp-progn body))
     (advice-remove 'straight--popup-raw '+minemacs--straight--popup-raw:around-a)
     (advice-remove 'read-string '+minemacs--read-string:around-a)
     (advice-remove 'y-or-n-p '+minemacs--y-or-n-p:around-a)))

(defun minemacs-bump-packages ()
  "Update MinEmacs packages to the last revisions (can cause breakages)."
  (interactive)
  (apply #'minemacs-load-module (minemacs-modules t)) ; Load all modules, include on-demand ones
  (+straight-with-no-questions
    (straight-pull-recipe-repositories) ; Update straight recipe repositories
    (straight-pull-all)
    (straight-freeze-versions)
    (straight-rebuild-all))
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
  (+straight-with-no-questions
    (straight-pull-recipe-repositories) ; Update straight recipe repositories
    (straight-thaw-versions)
    (straight-rebuild-all)) ; Rebuild the packages
  (minemacs-run-build-functions 'dont-ask)) ; Run package-specific build functions (ex: `pdf-tools-install')

(defun +straight-prune-build-cache ()
  "Prune straight.el build directories for old Emacs versions."
  (let* ((default-directory (file-name-concat straight-base-dir "straight/")))
    (straight-prune-build) ; Prune the build cache and build directory.
    (mapc ; Prune old build directories
     (+apply-partially-right #'+delete-file-or-directory 'trash 'recursive)
     (seq-filter
      (lambda (name)
        (not (member name (list straight-build-dir (concat straight-build-dir "-cache.el") "versions" "repos"))))
      (directory-files default-directory nil directory-files-no-dot-files-regexp)))))


(provide 'me-bootstrap)

;;; me-bootstrap.el ends here
