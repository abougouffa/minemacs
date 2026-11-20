;; me-bootstrap.el --- Bootstrap packages (package.el, package-vc & use-package) -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-09-17
;; Last modified: 2026-11-21

;;; Commentary:

;; Packages are declared with `use-package' and installed with `:ensure' or
;; `:vc'. Unlike the default `package' behavior, only the declared packages (and
;; their dependencies) are activated, so packages removed from the configuration
;; (or belonging to disabled modules) aren't loaded anymore. The installed newer
;; versions of built-in packages are activated early in "early-init.el".
;;
;; The default `use-package' implementation of `:ensure' and `:vc' checks
;; `package-installed-p', which is non-nil for built-in packages. Hence, it
;; never installs a newer version of a built-in package (ex. `transient',
;; `eglot', `org', etc.). In MinEmacs, a package is considered installed only
;; when it is present in `package-alist', so `:ensure t' and `:vc' work the same
;; way for built-in and external packages.

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

(defcustom +package-ignored-files '("test" "tests" "*-test.el" "*-tests.el")
  "Files and directories not to byte-compile when installing packages.
Test suites often require packages that aren't installed, which makes their
byte-compilation fail (`straight' didn't build them either). The patterns work
like the `:ignored-files' of a `package-vc' spec (which are honored too): a
pattern starting with \"/\" is relative to the package's directory, other
patterns match at any level."
  :group 'minemacs-core
  :type '(repeat string))

(defvar +package-autoloads-excluded-files nil
  "Alist of (PACKAGE . FILES), FILES are excluded when generating autoloads.")

(defun +package-exclude-from-autoloads (package &rest files)
  "Don't generate autoloads from FILES when installing PACKAGE."
  (setf (alist-get package +package-autoloads-excluded-files) files))

(defun +package-vc-allow-build-commands (&rest packages)
  "Allow `package-vc' to run `:make' and `:shell-command' for PACKAGES."
  (unless (eq package-vc-allow-build-commands t)
    (dolist (package packages)
      (add-to-list 'package-vc-allow-build-commands package))))



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

(defun +package-vc-reset (pkg-desc-or-dir rev)
  "Reset the Git checkout of PKG-DESC-OR-DIR to REV, staying on the same branch.
Local changes are stashed. Unlike checking out REV (which leaves the repository
in a \"detached HEAD\" state), this keeps `package-vc-upgrade' working."
  (let ((default-directory (if (stringp pkg-desc-or-dir) pkg-desc-or-dir (package-desc-dir pkg-desc-or-dir))))
    (unless (zerop (vc-git-command nil t nil "cat-file" "-e" (concat rev "^{commit}")))
      (vc-git-command nil 0 nil "fetch" "--quiet" "origin"))
    (vc-git-command nil t nil "stash" "push" "--quiet") ; Best effort
    (vc-git-command nil 0 nil "reset" "--quiet" "--hard" rev)))

(defun minemacs-packages-freeze ()
  "Save the versions of the installed packages to `minemacs-packages-lockfile'."
  (interactive)
  (let ((entries
         (cl-loop for (name desc) in (sort (copy-sequence package-alist) :key #'car)
                  collect (cons name (if (package-vc-p desc)
                                         (list :vc (or (plist-get (alist-get name package-vc-selected-packages) :url)
                                                       (vc-git-repository-url (package-desc-dir desc)))
                                               :rev (+package-vc-revision desc))
                                       (list :version (package-version-join (package-desc-version desc))))))))
    (make-directory (file-name-directory minemacs-packages-lockfile) t)
    (with-temp-file minemacs-packages-lockfile
      (insert ";; -*- mode: lisp-data; -*-\n"
              ";; MinEmacs packages lockfile, generated by `minemacs-packages-freeze', do not edit!\n\n"
              "(" (mapconcat #'prin1-to-string entries "\n ") ")\n"))
    (setq +package--lockfile entries)
    (message "[MinEmacs]: Saved %d packages versions to %S" (length entries) (abbreviate-file-name minemacs-packages-lockfile))))



;;; Installing and activating the declared packages

(defvar +package--locked-rev nil)

(defun +package--install-from-archive (name)
  "Install NAME from `package-archives', even if it is a built-in package."
  (unless package-archive-contents (package-refresh-contents)) ; Refresh once per session
  ;; When given a name, `package-install' doesn't install a newer version of a
  ;; built-in package, so we give it the package descriptor instead
  (package-install (or (cadr (assq name package-archive-contents))
                       (error "Package `%s' isn't available in the archives" name))))

(defun +package--install-from-vc (name opts rev)
  "Install NAME from VC using the `package-vc' spec OPTS and revision REV.
The revision saved in `minemacs-packages-lockfile' takes precedence over REV."
  (let ((+package--locked-rev (plist-get (+package-locked name) :rev)))
    (condition-case err
        (package-vc-install (if opts (cons name opts) name) (unless +package--locked-rev rev))
      (error ; Delete the partial checkout, otherwise, the next attempt fails
       (let ((dir (expand-file-name (symbol-name name) package-user-dir)))
         (when (and (file-directory-p dir) (not (assq name package-alist)))
           (delete-directory dir t)))
       (signal (car err) (cdr err))))))

(defun +package-ensure (name &optional vc)
  "Make sure the package NAME is installed, then activate it.
When VC is non-nil, it is a list (OPTIONS REVISION) as returned by
`use-package-normalize--vc-arg', and the package is installed with
`package-vc'. Otherwise, it is installed from `package-archives'."
  (unless minemacs-builtin-only-p
    (pcase-let ((`(,opts ,rev) vc))
      ;; Register the spec for `package-vc-upgrade' and `package-vc-rebuild'
      ;; (ex. the `:lisp-dir' of `transient'), it isn't persisted across sessions
      (when opts
        (setf (alist-get name package-vc-selected-packages) opts))
      (condition-case-unless-debug err
          (progn
            (unless (assq name package-alist) ; Built-in packages aren't in `package-alist'
              (if vc (+package--install-from-vc name opts rev) (+package--install-from-archive name)))
            (add-to-list 'package-selected-packages name) ; Keep it in `minemacs-packages-prune'
            (package-activate name))
        (error (display-warning 'minemacs (format "Cannot install or activate `%s': %s" name (error-message-string err)) :error))))))

(defun +use-package-ensure (name args _state &optional _no-refresh)
  "Ensure the packages ARGS of `use-package' NAME."
  (dolist (pkg args) ; `:ensure t', `:ensure PKG' or `:ensure (PKG :pin ARCHIVE)'
    (let ((pkg (if (eq pkg t) (use-package-as-symbol name) pkg)))
      (when (consp pkg)
        (use-package-pin-package (car pkg) (cdr pkg))
        (setq pkg (car pkg)))
      (when pkg (+package-ensure pkg)))))

(setq use-package-ensure-function #'+use-package-ensure)

;; Like the original handler, but use `+package-ensure' instead of
;; `use-package-vc-install' (which is inlined in Emacs 31+, so it can't be
;; advised). Packages are installed at runtime, even when byte-compiling.
(advice-add
 'use-package-handler/:vc :override
 (+defun +use-package-handler/:vc:override-a (name _keyword arg rest state)
   (cons `(+package-ensure ',(car arg) ',(cdr arg))
         (use-package-process-keywords name rest state))))

;; By default, `:ensure' is processed before `:disabled', `:if', `:when' and
;; `:unless', which installs disabled packages. Process it right after them.
(setq use-package-keywords (use-package-list-insert :ensure (remq :ensure use-package-keywords) :unless t))



;;; Fixes for `package' and `package-vc'

;; Pin the freshly cloned package to the locked revision before building it.
;; Passing the revision to `package-vc-install' checks it out in a "detached
;; HEAD" state, which breaks `package-vc-upgrade'.
(advice-add
 'package-vc--clone :after
 (+defun +package-vc--clone:after-a (_pkg-desc _pkg-spec dir _rev)
   (when +package--locked-rev (+package-vc-reset dir +package--locked-rev))))

;; FIX: `package-activate-1' reloads the already loaded files of a package before
;; adding it to `load-path', which fails when they require other files from the
;; package (ex. installing `compat' 31 after loading the built-in one).
(defvar find-library-source-path)

(defun +package-lisp-dirs (pkg-desc)
  "Return the directories containing the autoloads file of PKG-DESC.
This can be a sub-directory for packages installed from VC with a `:lisp-dir'."
  (let ((dir (package-desc-dir pkg-desc)))
    (or (delete-dups (mapcar #'file-name-directory
                             (directory-files-recursively dir (format "\\`%s-autoloads\\.el\\'" (package-desc-name pkg-desc))
                                                          nil (lambda (d) (not (string-suffix-p "/.git" d))))))
        (list dir))))

(advice-add
 'package--reload-previously-loaded :around
 (+defun +package--reload-previously-loaded:around-a (origfn pkg-desc &rest args)
   (let* ((find-library-source-path (or (bound-and-true-p find-library-source-path) load-path)) ; Look for loaded files in the original `load-path'
          (load-path (append (+package-lisp-dirs pkg-desc) load-path)))
     (apply origfn pkg-desc args))))

;; FIX: `package-strip-rcs-id' signals an error for free-form versions (ex.
;; "1.10  2009-2-25 hcz" in `gas-mode') instead of returning nil as documented.
;; `package-vc--version' can then return nil, so fallback to "0".
(advice-add 'package-strip-rcs-id :around (+defun +package-strip-rcs-id:around-a (origfn str) (ignore-errors (funcall origfn str))))
(advice-add 'package-vc--version :filter-return (+defun +package-vc--version:filter-return-a (version) (or version "0")))

;; Packages are built in the current session. Delaying the mode hooks prevents
;; the user's hooks (ex. `emacs-lisp-mode-hook') from running in the temporary
;; buffers used to extract the packages, generate the autoloads or build the
;; documentation. We change the default value, let-binding `delay-mode-hooks'
;; doesn't work as major modes make it buffer-local (and it causes "Making
;; delay-mode-hooks buffer-local while locally let-bound!" messages).
(+advice-add
 '(package-unpack package-vc--unpack-1) :around
 (+defun +package--delay-mode-hooks:around-a (origfn &rest args)
   (let ((orig-value (default-value 'delay-mode-hooks)))
     (unwind-protect
         (progn (setq-default delay-mode-hooks t)
                (apply origfn args))
       (setq-default delay-mode-hooks orig-value)
       ;; Buffers that changed their major mode in the meantime (ex. "*Compile-Log*")
       ;; got a buffer-local value of t, which would delay their mode hooks forever
       (dolist (buf (buffer-list))
         (when (and (local-variable-p 'delay-mode-hooks buf) (buffer-local-value 'delay-mode-hooks buf))
           (with-current-buffer buf (kill-local-variable 'delay-mode-hooks))))))))

;; Don't byte-compile the files matching `+package-ignored-files' or the
;; `:ignored-files' of the package's `package-vc' spec (by default, `package-vc'
;; only uses it to find dependencies). These are added to the patterns read from
;; the package's ".elpaignore" by `package--compile'.
(advice-add
 'package--parse-elpaignore :around
 (+defun +package--parse-elpaignore:around-a (origfn pkg-desc)
   (let ((dir (directory-file-name (package-desc-dir pkg-desc)))
         (spec (alist-get (package-desc-name pkg-desc) package-vc-selected-packages)))
     (append (funcall origfn pkg-desc)
             (mapcar (lambda (pattern) (wildcard-to-regexp (concat (if (string-prefix-p "/" pattern) dir "*/") pattern)))
                     (append +package-ignored-files (plist-get spec :ignored-files)))))))

;; Exclude some files from autoloads generation (see `+package-exclude-from-autoloads')
(advice-add
 'package-generate-autoloads :around
 (+defun +package-generate-autoloads:around-a (origfn name pkg-dir)
   (if-let* ((files (alist-get (if (stringp name) (intern name) name) +package-autoloads-excluded-files))
             (orig-loaddefs-generate (symbol-function 'loaddefs-generate)))
       (cl-letf (((symbol-function 'loaddefs-generate)
                  (lambda (dir output-file &optional excluded-files &rest args)
                    (apply orig-loaddefs-generate dir output-file
                           (append excluded-files (mapcar (+apply-partially-right #'expand-file-name pkg-dir) files))
                           args))))
         (funcall origfn name pkg-dir))
     (funcall origfn name pkg-dir))))



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
  (let ((package-selected-packages (seq-union package-selected-packages (mapcar #'car (+package-lockfile-read)))))
    (package-autoremove)))

(defun minemacs-packages-recompile ()
  "Byte-compile all the installed packages, like `package-recompile-all'.
All the installed packages are added to `load-path', so the dependencies of
the non-activated ones are found."
  (interactive)
  (let ((load-path (append (mapcan (lambda (elt) (+package-lisp-dirs (cadr elt))) package-alist) load-path)))
    (package-recompile-all)))

;; Byte-compiled files of a different Emacs version can be incompatible
(add-hook 'minemacs-after-emacs-version-change-hook #'minemacs-packages-recompile -90)

(provide 'me-bootstrap)

;;; me-bootstrap.el ends here
