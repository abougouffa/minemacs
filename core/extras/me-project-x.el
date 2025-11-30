;;; me-project-x.el --- Extensions for the builtin `project' -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-06-26
;; Last modified: 2025-11-30

;;; Commentary:

;;; Code:

;;;###autoload(with-eval-after-load 'project (require 'me-project-x))

;;; Thin `projectile' compatibility layer

;; Define some `projectile' wrapper functions on top of `project' (required by
;; some packages like `fzf', `neotree', `platformio-mode', etc.)

(require 'me-lib)

;;;###autoload
(progn
  (provide 'projectile)

  (defun projectile-project-p (&optional dir)
    (let ((default-directory (or dir default-directory)))
      (and (project-current) t)))

  (defun projectile-project-root (&optional dir)
    (when-let* ((default-directory (or dir default-directory))
                (proj (project-current)))
      (expand-file-name (project-root proj))))

  (defun projectile-project-name (&optional proj)
    (when-let* ((proj (or proj (project-current))))
      (project-name proj)))

  (defun projectile-project-files (&optional proj-root)
    (when-let* ((default-directory (or proj-root default-directory))
                (proj (project-current)))
      (mapcar #'file-relative-name (project-files proj))))

  (defun projectile-project-buffers (&optional proj)
    (when-let* ((proj (or proj (project-current))))
      (project-buffers proj)))

  (defun projectile-expand-root (name &optional dir)
    (when (projectile-project-p dir)
      (expand-file-name name (projectile-project-root dir))))

  (defun projectile-verify-file (file &optional dir)
    (when-let* ((file (projectile-expand-root file dir)))
      (file-exists-p file)))

  (defun projectile-project-buffer-p (buffer proj-root)
    (and (let ((default-directory proj-root))
           (member buffer (projectile-project-buffers)))
         t)))



;;; Find files x3.5 times faster using "fd", with optional caching for x1000
;;; faster listing

(defcustom +fd-program (if (executable-find "fdfind") "fdfind" "fd")
  "The fd program to use."
  :group 'minemacs-project
  :type 'string)

(defcustom +fd-ignores '("GPATH" "GRTAGS" "GTAGS" ".tags" ".ctags.d" "gtags.files" "cscope.files" "compile_commands.json")
  "List of files to ignore in \"fd\"."
  :group 'minemacs-project
  :type '(repeat string))

(defcustom +project-use-fd-on-remote t
  "Whether to use fd (if found) on remote machines' projects or not."
  :group 'minemacs-project
  :type 'boolean)

(defun +fd-ignores-arguments (ignores dir)
  "Like `xref--find-ignores-arguments', but for \"fd\"."
  (cl-assert (not (string-match-p "\\`~" dir)))
  (if ignores
      (concat " -E " (mapconcat #'shell-quote-argument ignores " -E "))
    ""))

(defun +fd-files-in-directory (dir ignores &optional files)
  "Find FILES (or all) using \"fd\" in directory DIR, excluding IGNORES.
Can override `project--files-in-directory' for x3.5 faster listing."
  (let* ((dir (file-name-as-directory dir))
         (default-directory dir)
         ;; Make sure ~/ etc. in local directory name is expanded and not left
         ;; for the shell command to interpret.
         (localdir (file-name-unquote (file-local-name (expand-file-name dir))))
         (command (format "%s -c never -H %s -t f -0 --strip-cwd-prefix %s"
                          +fd-program
                          (+fd-ignores-arguments (append ignores +fd-ignores) "./")
                          (if files
                              (concat (shell-quote-argument "(")
                                      (shell-quote-argument
                                       (mapconcat (lambda (wildcard)
                                                    (thread-last
                                                      (wildcard-to-regexp wildcard)
                                                      (string-remove-suffix "\\'")
                                                      (string-remove-prefix "\\`")))
                                                  (string-split files)
                                                  (concat "|")))
                                      (shell-quote-argument ")"))
                            "")))
         res)
    (with-temp-buffer
      (let ((status (process-file-shell-command command nil t)))
        (unless (zerop status)
          (goto-char (point-min))
          (if (and (not (eql status 127)) (search-forward "Permission denied\n" nil t))
              (let ((end (1- (point))))
                (re-search-backward "\\`\\|\0")
                (error "File listing failed: %s" (buffer-substring (1+ (point)) end)))
            (error "File listing failed: %s" (buffer-string))))
        (setq res (string-split (buffer-substring-no-properties (point-min) (point-max)) "\0" t))))
    (if project-files-relative-names
        (sort res #'string<)
      (project--remote-file-names
       (mapcar (apply-partially #'concat localdir)
               (sort res #'string<))))))

;; x3.5 faster than the default
(defun +project--files-in-directory-faster (orig-fn dir ignores &optional files)
  "Like `project--files-in-directory', uses \"fd\" with optional caching."
  (let ((+fd-program (let ((default-directory dir)
                           (remote (and (file-remote-p dir) t)))
                       (and (or (not remote) +project-use-fd-on-remote)
                            (seq-some (+apply-partially-right #'executable-find remote)
                                      (seq-uniq `(,+fd-program "fd" "fdfind"))))))
        (find-fn (if +fd-program #'+fd-files-in-directory orig-fn)))
    (funcall find-fn dir ignores files)))

(advice-add 'project--files-in-directory :around '+project--files-in-directory-faster)



;;; Memoization

(+memoize-function project--value-in-dir)
(+memoize-function project--files-in-directory)
(+memoize-function project-current project-current-directory-override default-directory)

;;; Fixes

;; When opening a project via Tramp, calling this can trigger "File is missing: /ssh:.../.gitmodules"
(advice-add
 'project--git-submodules :around
 (satch-defun +project--git-submodules-check-file:around-a (fn)
   (when (file-exists-p ".gitmodules") (funcall fn))))



;;; Project-based minor modes (stolen from Doom Emacs)

(defun +files--build-checks (spec &optional directory)
  "Converts a simple nested series of or/and forms into a series of
`file-exists-p' checks.

For example

  (+files--build-checks
    '(or A (and B C))
    \"~\")

Returns (not precisely, but effectively):

  '(let* ((_directory \"~\")
          (A (expand-file-name A _directory))
          (B (expand-file-name B _directory))
          (C (expand-file-name C _directory)))
     (or (and (file-exists-p A) A)
         (and (if (file-exists-p B) B)
              (if (file-exists-p C) C))))

This is used by `+file-exists-p!' and `+project-file-exists-p!'."
  (declare (pure t) (side-effect-free t))
  (if (and (listp spec)
           (memq (car spec) '(or and)))
      (cons (car spec)
            (cl-loop for it in (cdr spec)
                     collect (+files--build-checks it directory)))
    (let ((filevar (make-symbol "file")))
      `(let ((,filevar ,spec))
         (and (stringp ,filevar)
              ,(if directory
                   `(let ((default-directory ,directory))
                      (file-exists-p ,filevar))
                 `(file-exists-p ,filevar))
              ,filevar)))))

;;;###autoload
(defmacro +file-exists-p! (files &optional directory)
  "Returns non-nil if the FILES in DIRECTORY all exist.

DIRECTORY is a path; defaults to `default-directory'.

Returns the last file found to meet the rules set by FILES, which can be a
single file or nested compound statement of `and' and `or' statements."
  `(let ((p ,(+files--build-checks files directory)))
     (and p (expand-file-name p ,directory))))

;;;###autoload
(defmacro +project-file-exists-p! (files &optional base-directory)
  "Checks if FILES exist at the current project's root.

The project's root is determined by `projectile', starting from BASE-DIRECTORY
(defaults to `default-directory'). FILES are paths relative to the project root,
unless they begin with a slash."
  `(+file-exists-p! ,files (+project-root-for-dir ,base-directory)))

(defvar minemacs-project-hook nil
  "A hook that runs when a project is enabled.
The name of the project's mode and its state are passed in.")

(cl-defmacro +def-project-mode! (name &key modes files when match add-hooks on-load on-enter on-exit)
  "Define a project minor mode named NAME and where/how it is activated.

Project modes allow you to configure 'sub-modes' for major-modes that are
specific to a folder, project structure, framework or whatever arbitrary context
you define. These project modes can have their own settings, keymaps, hooks,
snippets, etc.

This creates NAME-hook and NAME-map as well.

PLIST may contain any of these properties, which are all checked to see if NAME
should be activated. If they are *all* true, NAME is activated.

  :modes MODES -- if buffers are derived from MODES (one or a list of symbols).

  :files FILES -- if project contains FILES; takes a string or a form comprised
    of nested (and ...) and/or (or ...) forms. Each path is relative to the
    project root, however, if prefixed with a '.' or '..', it is relative to the
    current buffer.

  :match REGEXP -- if file name matches REGEXP.

  :when PREDICATE -- if PREDICATE returns true (can be a form or the symbol of a
    function).

  :add-hooks HOOKS -- HOOKS is a list of hooks to add this mode's hook.

  :on-load FORM -- FORM to run the first time this project mode is enabled.

  :on-enter FORM -- FORM is run each time the mode is activated.

  :on-exit FORM -- FORM is run each time the mode is disabled.

Relevant: `minemacs-project-hook'."
  (declare (indent 1))
  (let ((init-var (intern (format "%s-init" name))))
    (macroexp-progn
     (append
      (when on-load
        `((defvar ,init-var nil)))
      `((define-minor-mode ,name
          "A project minor mode generated by `+def-project-mode!'."
          :init-value nil
          :lighter ""
          :keymap (make-sparse-keymap)
          (if (not ,name)
              ,on-exit
            (run-hook-with-args 'minemacs-project-hook ',name ,name)
            ,(when on-load
               `(unless ,init-var
                  ,on-load
                  (setq ,init-var t)))
            ,on-enter))
        (dolist (hook ,add-hooks)
          (add-hook ',(intern (format "%s-hook" name)) hook)))
      (cond ((or files modes when)
             (cl-check-type files (or null list string))
             (let ((fn
                    `(lambda ()
                       (and (not (bound-and-true-p ,name))
                            (and buffer-file-name (not (file-remote-p buffer-file-name nil t)))
                            ,(or (null match)
                                 `(if buffer-file-name (string-match-p ,match buffer-file-name)))
                            ,(or (null files)
                                 ;; Wrap this in `eval' to prevent eager expansion
                                 ;; of `+project-file-exists-p!' from pulling in
                                 ;; autoloaded files prematurely.
                                 `(eval
                                   '(+project-file-exists-p!
                                     ,(if (stringp (car files)) (cons 'and files) files))))
                            ,(or when t)
                            (,name 1)))))
               (if modes
                   `((dolist (mode ,modes)
                       (let ((hook-name
                              (intern (format "minemacs--enable-%s%s-h" ',name
                                              (if (eq mode t) "" (format "-in-%s" mode))))))
                         (fset hook-name #',fn)
                         (if (eq mode t)
                             (add-to-list 'auto-minor-mode-magic-alist (cons hook-name #',name))
                           (add-hook (intern (format "%s-hook" mode)) hook-name)))))
                 `((add-hook 'change-major-mode-after-body-hook #',fn)))))
            (match
             `((add-to-list 'auto-minor-mode-alist (cons ,match #',name)))))))))


(provide 'me-project-x)
;;; me-project-x.el ends here
