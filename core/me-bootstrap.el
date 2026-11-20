;; me-bootstrap.el --- Bootstrap packages (package.el, package-vc & use-package) -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-09-17
;; Last modified: 2026-11-20

;;; Commentary:

;;; Code:

(require 'me-lib)
(require 'package)
(require 'package-vc)
(require 'vc-git)
(require 'use-package)
(require 'use-package-ensure)

(defcustom minemacs-packages-lockfile (concat minemacs-local-dir "packages/versions.el")
  "The file in which the installed packages versions are saved."
  :group 'minemacs-core
  :type 'file)

(defvar +package-autoloads-excluded-files nil
  "Alist of (PACKAGE . FILES), FILES are excluded when generating autoloads.
This is the equivalent of `straight's \":files (:defaults (:exclude ...))\".")



;;; Activation and session state

;; Like with `straight', a package (and its dependencies) is only activated when
;; declared with `:ensure' or `:vc', so packages removed from the config aren't
;; loaded anymore. Packages installed manually are available for the current
;; session only. Newer versions of built-in packages are activated in
;; "early-init.el".

(defvar minemacs-declared-packages nil
  "The packages declared with `:ensure' or `:vc' in the current session.")

(defun +package-lisp-dirs (pkg-desc)
  "Return the directories containing the Lisp files of PKG-DESC.
This is where the autoloads file is located, it can be a sub-directory for
packages installed from VC with a `:lisp-dir' (like `transient')."
  (let ((dir (package-desc-dir pkg-desc))
        (autoloads (format "\\`%s-autoloads\\.el\\'" (regexp-quote (symbol-name (package-desc-name pkg-desc))))))
    (or (delete-dups (mapcar #'file-name-directory
                             (directory-files-recursively dir autoloads nil (lambda (d) (not (string-suffix-p "/.git" d))))))
        (list dir))))

;; FIX: `package-activate-1' reloads the already loaded files of a package before
;; adding it to `load-path', which fails when they require other files from the
;; package (ex. installing `compat' 31 after loading the built-in one).
(defvar find-library-source-path)

(advice-add
 'package--reload-previously-loaded :around
 (+defun +package--reload-previously-loaded:around-a (origfn pkg-desc &rest args)
   ;; Keep looking for the loaded files in the original `load-path'
   (let* ((find-library-source-path (or (bound-and-true-p find-library-source-path) load-path))
          (load-path (append (+package-lisp-dirs pkg-desc) load-path)))
     (apply origfn pkg-desc args))))

(defun +package-activate (&rest packages)
  "Activate the installed PACKAGES and their dependencies.
PACKAGES are registered in `minemacs-declared-packages' and, for this session,
in `package-selected-packages'. Packages that aren't installed are ignored."
  (dolist (package packages)
    (when (and package (symbolp package))
      (add-to-list 'minemacs-declared-packages package t)
      (add-to-list 'package-selected-packages package)
      (unless (memq package package-activated-list)
        (condition-case-unless-debug err
            (package-activate package)
          (error (display-warning 'minemacs (format "Failed to activate `%s': %s" package (error-message-string err)) :error)))))))

;; HACK: Calling `package-activate-all' (or `package-initialize') shouldn't
;; activate all the installed packages.
(advice-add
 'package-activate-all :override
 (+defun +package-activate-all:override-a ()
   (setq package--activated t)
   (+log! "Ignoring `package-activate-all', only the declared packages are activated")))

;; FIX: `package-strip-rcs-id' signals an error for free-form versions (ex.
;; "1.10  2009-2-25 hcz" in `gas-mode') instead of returning nil as documented.
;; Use the leading version number when available.
(advice-add
 'package-strip-rcs-id :around
 (+defun +package-strip-rcs-id:around-a (origfn str)
   (condition-case nil
       (funcall origfn str)
     (error (when-let* (((stringp str))
                        ((string-match "\\`[ \t]*\\(?:[$]Revision:[ \t]+\\)?\\([0-9]+\\(?:\\.[0-9]+\\)*\\)" str)))
              (ignore-errors (funcall origfn (match-string 1 str))))))))

;; Fallback to "0" when the version can't be parsed, like when it is missing
(advice-add
 'package-vc--version :filter-return
 (+defun +package-vc--version:filter-return-a (version) (or version "0")))

;; Unlike `straight' which builds packages in a separate process, `package'
;; builds them in the current session. Delaying the mode hooks prevents the
;; user's configuration hooks (ex. `prog-mode-hook') from running in the buffers
;; used when generating the autoloads, compiling or building the documentation.
(+advice-add
 '(package-unpack package-vc--unpack-1) :around
 (+defun +package--delay-mode-hooks:around-a (origfn &rest args)
   (let ((delay-mode-hooks t)) (apply origfn args))))



;;; Lockfile

(defvar +package--lockfile 'unset)

(defun +package-lockfile-read ()
  "Return the content of `minemacs-packages-lockfile'."
  (setq +package--lockfile
        (when (file-exists-p minemacs-packages-lockfile)
          (with-temp-buffer
            (insert-file-contents minemacs-packages-lockfile)
            (ignore-errors (read (current-buffer)))))))

(defun +package-locked (package)
  "Get the locked properties of PACKAGE, a plist like (:rev REV :version VER)."
  (alist-get package (if (eq +package--lockfile 'unset) (+package-lockfile-read) +package--lockfile)))

(defun +package-vc-revision (pkg-desc)
  "Return the current revision of the VC package PKG-DESC.
Like `package-vc-commit', but bypass VC's cache which can be stale after
pulling or resetting a package in the same session."
  (vc-clear-context)
  (package-vc-commit pkg-desc))

(defun minemacs-packages-freeze ()
  "Save the versions of the installed packages to `minemacs-packages-lockfile'."
  (interactive)
  (let ((entries
         (cl-loop for (name desc) in (sort (copy-sequence package-alist) :key #'car)
                  collect (cons name (if (package-vc-p desc)
                                         (let ((dir (package-desc-dir desc)))
                                           (list :vc (or (plist-get (alist-get name package-vc-selected-packages) :url)
                                                         (vc-git-repository-url dir))
                                                 :rev (+package-vc-revision desc)))
                                       (list :version (package-version-join (package-desc-version desc))))))))
    (make-directory (file-name-directory minemacs-packages-lockfile) t)
    (with-temp-file minemacs-packages-lockfile
      (insert ";; -*- mode: lisp-data; -*-\n"
              ";; MinEmacs packages lockfile, generated by `minemacs-packages-freeze', do not edit!\n\n"
              "(" (mapconcat #'prin1-to-string entries "\n ") ")\n"))
    (setq +package--lockfile entries)
    (message "[MinEmacs]: Saved %d packages versions to %S" (length entries) (abbreviate-file-name minemacs-packages-lockfile))))

(defun +package-vc-reset (pkg-desc-or-dir rev)
  "Reset the Git checkout of PKG-DESC-OR-DIR to REV, staying on the same branch.
Local changes are stashed. Unlike checking out REV (which leaves the repository
in a \"detached HEAD\" state), this keeps `package-vc-upgrade' working."
  (let ((default-directory (if (stringp pkg-desc-or-dir) pkg-desc-or-dir (package-desc-dir pkg-desc-or-dir))))
    (unless (zerop (vc-git-command nil t nil "cat-file" "-e" (concat rev "^{commit}")))
      (vc-git-command nil 0 nil "fetch" "--quiet" "origin"))
    (vc-git-command nil t nil "stash" "push" "--quiet") ; Best effort
    (vc-git-command nil 0 nil "reset" "--quiet" "--hard" rev)))



;;; Hooking `package' into `use-package'

;; Unlike `use-package-vc-install', `+package-vc-install':
;; - Ignores built-in packages, to allow installing a newer `transient'
;; - Installs the locked revision, if any (see `+package-vc--clone:after-a')
;; - Registers the spec in `package-vc-selected-packages' for
;;   `package-vc-rebuild'/`package-vc-upgrade' (ex. `transient's `:lisp-dir')
;; - Shows a warning instead of signaling an error on failure
(defvar +package--pinned-rev nil)

(defun +package-vc-install (arg &optional local-path)
  "Install the package described by ARG with `package-vc'.
ARG is a list (NAME OPTIONS REVISION) as returned by
`use-package-normalize--vc-arg'. When LOCAL-PATH is non-nil, install from
the local checkout in LOCAL-PATH."
  (pcase-let ((`(,name ,opts ,rev) arg))
    (when opts (setf (alist-get name package-vc-selected-packages) opts))
    (unless (assq name package-alist) ; Already installed
      (let* ((dir (expand-file-name (symbol-name name) package-user-dir))
             (dir-existed (file-directory-p dir)))
        (unwind-protect
            (condition-case-unless-debug err
                (if local-path
                    (with-no-warnings (package-vc-install-from-checkout local-path (symbol-name name)))
                  (let ((+package--pinned-rev (plist-get (+package-locked name) :rev)))
                    (package-vc-install (if opts (cons name opts) name) (unless +package--pinned-rev rev))))
              (error (display-warning 'minemacs (format "Failed to install `%s' from VC: %s" name (error-message-string err)) :error)))
          ;; On failure, delete the partial checkout (or the next attempt fails)
          (when (and (not local-path) (not dir-existed) (not (assq name package-alist)) (file-directory-p dir))
            (delete-directory dir t)))))))

;; Pin the freshly cloned package to the locked revision before building it
(advice-add
 'package-vc--clone :after
 (+defun +package-vc--clone:after-a (_pkg-desc _pkg-spec dir _rev)
   (when +package--pinned-rev (+package-vc-reset dir +package--pinned-rev))))

;; Activate the packages declared with `:ensure' and `:vc'. This is done in the
;; expanded code, so it works for byte-compiled forms too. Installing a package
;; activates it, so the activation can come before the installation.
(advice-add
 'use-package-handler/:ensure :around
 (+defun +use-package-handler/:ensure:around-a (origfn name keyword ensure rest state)
   (let ((body (funcall origfn name keyword ensure rest state)))
     (if-let* (((not (plist-member rest :vc))) ; Handled by `:vc'
               (packages (delq nil (mapcar (lambda (elt) ; `:ensure t', `:ensure PKG' or `:ensure (PKG :pin ARCHIVE)'
                                             (let ((pkg (if (eq elt t) (use-package-as-symbol name) elt)))
                                               (if (consp pkg) (car pkg) pkg)))
                                           ensure))))
         (cons `(+package-activate ,@(mapcar #'macroexp-quote packages)) body)
       body))))

;; HACK: Override the `:vc' handler, as advising `use-package-vc-install' doesn't
;; work when it is inlined (it uses `define-inline' in Emacs 32).
(advice-add
 'use-package-handler/:vc :override
 (+defun +use-package-handler/:vc:override-a (name _keyword arg rest state)
   (let ((body (use-package-process-keywords name rest state))
         (local-path (car (plist-get state :load-path))))
     ;; Like `use-package-handler/:vc', install when byte-compiling
     (if (bound-and-true-p byte-compile-current-file)
         (+package-vc-install arg local-path)
       (push `(+package-vc-install ',arg ,local-path) body))
     (cons `(+package-activate ',(car arg)) body))))

;; Exclude some files from autoloads generation, the equivalent of `straight's
;; ":files (:defaults (:exclude ...))"
(advice-add
 'package-generate-autoloads :around
 (+defun +package-generate-autoloads:around-a (origfn name pkg-dir)
   (if-let* ((files (alist-get (if (stringp name) (intern name) name) +package-autoloads-excluded-files))
             (orig-loaddefs-generate (symbol-function 'loaddefs-generate)))
       (cl-letf (((symbol-function 'loaddefs-generate)
                  (lambda (dir output-file &optional excluded-files &rest args)
                    (apply orig-loaddefs-generate dir output-file
                           (append excluded-files (mapcar (lambda (file) (expand-file-name file pkg-dir)) files))
                           args))))
         (funcall origfn name pkg-dir))
     (funcall origfn name pkg-dir))))

;; Like `straight's default recipe files (":defaults"), only byte-compile the
;; top-level Lisp files of packages installed from VC, and skip test files. This
;; avoids compiling the tests, examples, etc. which can fail to compile.
(advice-add
 'package--parse-elpaignore :around
 (+defun +package--parse-elpaignore:around-a (origfn pkg-desc)
   (let* ((regexps (funcall origfn pkg-desc))
          (dir (file-name-as-directory (expand-file-name (package-desc-dir pkg-desc))))
          (git-root (locate-dominating-file dir ".git")))
     (if (and git-root (file-in-directory-p git-root package-user-dir)) ; A VC package
         (append (list (concat "\\`" (regexp-quote dir) "[^/]+/") ; Files in sub-directories
                       "\\(?:\\`\\|/\\)\\(?:tests?\\|[^/]*-tests?\\)\\.el\\'") ; Test files
                 regexps)
       regexps))))

(defun +package-exclude-from-autoloads (package &rest files)
  "Don't generate autoloads from FILES when installing PACKAGE."
  (setf (alist-get package +package-autoloads-excluded-files) files))

(defun +package-vc-allow-build-commands (&rest packages)
  "Allow `package-vc' to run `:make' and `:shell-command' for PACKAGES."
  (unless (eq package-vc-allow-build-commands t)
    (dolist (package packages)
      (add-to-list 'package-vc-allow-build-commands package))))

;; HACK: This advice around `use-package' checks if a package is disabled in
;; `minemacs-disabled-packages' before calling `use-package'. This can come
;; handy if the user wants to enable some module while excluding some packages
;; from it. This advice also evaluates `use-package's conditional sections
;; (`:if', `:when' and `:unless') to prevent installing packages with
;; `:ensure' or `:vc' when they aren't needed.
(advice-add
 'use-package :around
 (+defun +use-package--check-if-disabled:around-a (origfn package &rest args)
   (unless (and +use-package-check-for-disabled
                (or (+package-disabled-p package)
                    (memq :disabled args)
                    (and (memq :if args) (not (eval (+varplist-get args :if t))))
                    (and (memq :when args) (not (eval (+varplist-get args :when t))))
                    (and (memq :unless args) (eval (+varplist-get args :unless t)))))
     ;; Add it to the list of configured packages and apply the `use-package' form
     (add-to-list 'minemacs-configured-packages package t)
     (apply origfn package args))))



;;; Updating packages

(defun +package-vc-upgrade (pkg-desc)
  "Like `package-vc-upgrade' for PKG-DESC, but wait for it to finish.
`package-vc-upgrade' pulls asynchronously, which doesn't work in batch mode."
  (let ((procs (process-list)))
    (package-vc-upgrade pkg-desc)
    (while-let ((new-procs (seq-filter (lambda (proc) (and (process-live-p proc) (not (memq proc procs))))
                                       (process-list))))
      (accept-process-output (car new-procs) 0.5))))

(defun minemacs-bump-packages ()
  "Update MinEmacs packages to the last revisions (can cause breakages)."
  (interactive)
  (apply #'minemacs-load-module (minemacs-modules t)) ; Load all modules, include on-demand ones
  (package-upgrade-all nil) ; Refresh the archives and upgrade the packages installed from them
  (dolist (elt package-alist) ; Pull the packages installed from VC
    (when (package-vc-p (cadr elt)) (+package-vc-upgrade (cadr elt))))
  (minemacs-packages-freeze)
  (minemacs-run-build-functions)) ; Run package-specific build functions (ex: `pdf-tools-install')

(defun minemacs-bump-packages-async ()
  "Like `minemacs-bump-packages', but runs asynchronously."
  (interactive)
  (let* ((compilation-buffer-name-function (lambda (&rest _args) "*minemacs-bump-packages*")))
    (compile (concat (shell-quote-argument (car command-line-args))
                     " --batch --eval='(minemacs-bump-packages)' --script "
                     (shell-quote-argument user-init-file)))))

(defun minemacs-upgrade (pull)
  "Upgrade the packages to the versions saved in `minemacs-packages-lockfile'.
Packages installed from VC are restored to the locked revisions. As the
package archives only serve the latest version of each package, packages
installed from the archives are only upgraded when they are older than the
locked version. When called with \\[universal-argument] or with PULL, it
will run \"git pull\" in MinEmacs directory before upgrading."
  (interactive "P")
  (when pull (let ((default-directory minemacs-root-dir)) (vc-pull)))
  (package-refresh-contents)
  (pcase-dolist (`(,name . ,locked) (+package-lockfile-read))
    (when-let* ((desc (cadr (assq name package-alist))))
      (condition-case-unless-debug err
          (if (package-vc-p desc)
              (when-let* ((rev (plist-get locked :rev))
                          ((not (equal rev (+package-vc-revision desc)))))
                (+package-vc-reset desc rev)
                (package-vc-rebuild desc))
            (when-let* ((version (plist-get locked :version))
                        ((version-list-< (package-desc-version desc) (version-to-list version))))
              (package-upgrade name)))
        (error (+error! "Cannot restore `%s': %s" name (error-message-string err))))))
  (minemacs-run-build-functions)) ; Run package-specific build functions (ex: `pdf-tools-install')

(defun minemacs-packages-prune ()
  "Delete the installed packages that aren't used by MinEmacs nor by the user.
Packages declared in this session or listed in the lockfile, and their
dependencies, are kept."
  (interactive)
  (let ((package-selected-packages
         (seq-union package-selected-packages (mapcar #'car (+package-lockfile-read)))))
    (package-autoremove)))

(defun minemacs-packages-recompile ()
  "Byte-compile all the installed packages, like `package-recompile-all'.
All the installed packages are added to `load-path', so the dependencies of
the non-activated ones are found."
  (interactive)
  (let ((load-path (append (mapcan (lambda (elt) (+package-lisp-dirs (cadr elt))) package-alist) load-path)))
    (package-recompile-all)))

;; Byte-compiled files of a different Emacs version can be incompatible
;; (`straight' used to have a build directory per Emacs version)
(add-hook 'minemacs-after-emacs-version-change-hook #'minemacs-packages-recompile -90)

(provide 'me-bootstrap)

;;; me-bootstrap.el ends here
