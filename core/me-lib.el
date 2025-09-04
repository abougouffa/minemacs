;; me-lib.el -- MinEmacs Library (helper functions, extra features and commands) -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2023-11-29
;; Last modified: 2025-09-04

;;; Commentary:

;;; Code:

(require 'me-vars)
(require 'cl-lib)
(require 'subr-x)
(require 'rx)

(autoload 'cl-loop "cl-macs" nil nil 'macro)



(defvar minemacs--lazy-low-priority-forms nil)
(defvar minemacs--lazy-high-priority-forms nil)

(defmacro +with-delayed! (&rest body)
  "Delay evaluating BODY with priority 0 (high priority)."
  (declare (indent 0))
  `(push ',(macroexp-progn body) minemacs--lazy-high-priority-forms))

(defmacro +with-delayed-1! (&rest body)
  "Delay evaluating BODY with priority 1."
  (declare (indent 0))
  `(push ',(macroexp-progn body) minemacs--lazy-low-priority-forms))



;;; Some plist and alist missing functions

(defun +varplist-get (vplist keyword &optional car-p)
  "Get KEYWORD's value from variable value length VPLIST.
Ex: (+varplist-get \\='(:a \\='a :b \\='b1 \\='b2) :b) -> \\='(b1 b2)."
  (funcall
   (if car-p #'cadr #'cdr)
   (cl-loop for element in (memq keyword vplist)
            until (and (not (eq element keyword)) (keywordp element))
            collect element)))

(defun +plist-keys (plist)
  "Return the keys of PLIST."
  (let (keys)
    (while plist
      (push (car plist) keys)
      (setq plist (cddr plist)))
    keys))

;; Taken from: https://emacs.stackexchange.com/q/33892/12534
(defun +alist-set (key val alist &optional symbol)
  "Set property KEY to VAL in ALIST. Return new alist.
This creates the association if it is missing, and otherwise sets the cdr of the
first matching association in the list. It does not create duplicate
associations. By default, key comparison is done with `equal'. However, if
SYMBOL is non-nil, then `eq' is used instead.

This method may mutate the original alist, but you still need to use the return
value of this method instead of the original alist, to ensure correct results."
  (if-let* ((pair (if symbol (assq key alist) (assoc key alist))))
      (setcdr pair val)
    (push (cons key val) alist))
  alist)

(defun +add-to-list-at (list-var element index)
  "Insert into LIST-VAR an ELEMENT at INDEX.
If INDEX is 0, ELEMENT is inserted before the first element."
  (let* ((padded-list (cons nil (eval list-var)))
         (c (nthcdr index padded-list)))
    (setcdr c (cons element (cdr c)))
    (set list-var (cdr padded-list))))

;;; Missing primitive utilities

;; Adapted from `evil-unquote', takes functions into account
(defun +unquote (expr)
  "Return EXPR unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe expr) '(quote function))
    (setq expr (cadr expr)))
  expr)

(defun +apply-partially-right (fun &rest args)
  "Like `apply-partially', but apply the ARGS to the right of FUN."
  (lambda (&rest args2)
    (apply fun (append args2 args))))

(defun +reverse-args (fun)
  "Return a function that calls FUN with arguments in the reversed order."
  (lambda (&rest args)
    (apply fun (reverse args))))



;;; Minemacs' core functions and macros

(defmacro +error! (msg &rest vars)
  "Log error MSG and VARS using `message'."
  (when (>= minemacs-msg-level 1)
    `(apply #'message (list (concat "[MinEmacs:Error] " ,msg) ,@vars))))

(defmacro +info! (msg &rest vars)
  "Log info MSG and VARS using `message'."
  (when (>= minemacs-msg-level 2)
    `(let ((inhibit-message t))
       (apply #'message (list (concat "[MinEmacs:Info] " ,msg) ,@vars)))))

(defmacro +log! (msg &rest vars)
  "Log MSG and VARS using `message' when `minemacs-verbose-p' is non-nil."
  (when (>= minemacs-msg-level 3)
    `(let ((inhibit-message t))
       (apply #'message (list (concat "[MinEmacs:Log] " ,msg) ,@vars)))))

(defmacro +debug! (msg &rest vars)
  "Log debug MSG and VARS using `message' when `minemacs-msg-level' is 4."
  (when (>= minemacs-msg-level 4)
    `(let ((inhibit-message t))
       (apply #'message (list (concat "[MinEmacs:Debug] " ,msg) ,@vars)))))

(defmacro +shutup! (&rest body)
  "Suppress new messages temporarily while evaluating BODY.
This inhebits both the echo area and the `*Messages*' buffer. If `:log' is
provided as the first argument, inhibit messages but keep writing them to the
`*Messages*' buffer."
  (let* ((logp (eq :log (car body)))
         (body (if logp (cdr body) body)))
    (if (not minemacs-verbose-p)
        `(let ((message-log-max ,(when logp message-log-max))
               (inhibit-message t))
           (with-temp-message (or (current-message) "") ,@body))
      `(progn ,@body))))

(defun +apply-inhibit-messages (fn &rest args) ; Helper functions to be used as advises
  "Call FN with ARGS while to suppressing the messages in echo area.
If `minemacs-verbose-p' is non-nil, do not print any message to
*Messages* buffer."
  (if (called-interactively-p 'interactive)
      (apply fn args)
    (let ((message-log-max (and minemacs-verbose-p message-log-max)))
      (with-temp-message (or (current-message) "")
        (+debug! "Inhibiting messages of %s" (symbol-name fn))
        (apply fn args)))))

(defun +apply-suppress-messages (fn &rest args) ; Helper functions to be used as advises
  "Call FN with ARGS while to suppressing the messages in echo area.
The messages are still printed to *Messages* buffer."
  (if (called-interactively-p 'interactive)
      (apply fn args)
    (with-temp-message (or (current-message) "")
      (apply fn args))))

(defun +load-theme ()
  "Load Emacs' theme from `minemacs-theme'."
  (interactive)
  (when minemacs-theme
    (+log! "Loading user theme: %s" minemacs-theme)
    ;; Fallback to built-in `tsdh-light' when `minemacs-theme' is not available.
    (condition-case nil
        (load-theme minemacs-theme t)
      (error
       (let ((default-theme (eval (car (get 'minemacs-theme 'standard-value)))))
         (+log! "Cannot load theme %S, trying to load the default theme %S" minemacs-theme default-theme)
         (condition-case nil
             (load-theme default-theme t)
           (error
            (+log! "Cannot load default theme %S, falling back to the builtin `modus-operandi' theme" default-theme)
            (load-theme 'modus-operandi t)))))))
  ;; Run hooks
  (run-hooks 'minemacs-after-load-theme-hook))

;; An internal variable to keep track of the tasks
(defvar +eval-when-idle--task-num 0)
(defcustom +eval-when-idle-delay 5.0
  "The default delay (in seconds) to consider in `+eval-when-idle!' macro."
  :group 'minemacs-core
  :type 'float)

(defcustom +lazy-delay 1.0
  "The default delay (in seconds) to consider in `+lazy!' macro."
  :group 'minemacs-core
  :type 'float)

(defun +eval-when-idle (delay &rest fns)
  "Queue FNS to be processed when Emacs becomes idle after DELAY seconds."
  (let* ((task-num (cl-incf +eval-when-idle--task-num))
         (task-name (make-symbol (format "+eval-when-idle--task-%d" task-num))))
    (with-memoization (get task-name 'timer)
      (run-with-idle-timer
       delay t
       (lambda ()
         (+log! "Running task %d, calling function `%s'" task-num (truncate-string-to-width (format "%s" (car fns)) 40 nil nil "…"))
         (funcall (pop fns))
         (unless fns
           (cancel-timer (get task-name 'timer))
           (put task-name 'timer nil)))))))

(defmacro +eval-when-idle! (&rest body)
  "Evaluate BODY when Emacs becomes idle."
  (declare (indent 0))
  `(+eval-when-idle ,+eval-when-idle-delay (lambda () ,@body)))

(defmacro +eval-when-idle-for! (delay &rest body)
  "Evaluate BODY after DELAY seconds from Emacs becoming idle."
  (declare (indent 1))
  `(+eval-when-idle ,delay (lambda () ,@body)))

(defmacro +deferred! (&rest body)
  "Run BODY after Emacs gets loaded, a.k.a. after `minemacs-loaded'."
  `(with-eval-after-load 'minemacs-loaded ,@body))

(defmacro +lazy! (&rest body)
  "Run BODY as a lazy block (see `minemacs-lazy')."
  `(with-eval-after-load 'minemacs-lazy
     (+eval-when-idle-for! +lazy-delay ,@body)))

(defcustom +first-file-hook-ignore-list nil
  "A list of files to ignore in the `minemacs-first-*-file-hook' hooks.

It can be a filename, a filename with wildcard or a function that
returns one of the two."
  :group 'minemacs-core
  :type '(repeat (choice function string)))

(defcustom +first-file-hooks nil
  "A list of hooks defined using `+make-first-file-hook!'."
  :group 'minemacs-core
  :type '(repeat symbol))

(defmacro +make-first-file-hook! (filetype ext-regexp)
  "Make a hook which run on the first FILETYPE file of a particular extensions.
The extension should matches EXT-REGEXP.

This will creates a function named `+first-file--FILETYPE-h' which gets executed
before `after-find-file'. This function will run on the first file that matches
EXT-REGEXP. When it runs, this function provides a feature named
`minemacs-first-FILETYPE-file' and a run all hooks in
`minemacs-first-FILETYPE-file-hook'."
  (let* ((filetype (+unquote filetype))
         (fn-name (intern (format "+first-%s-file:after-a" (if filetype (format "-%s" filetype) ""))))
         (hook-name (intern (format "minemacs-first%s-file-hook" (if filetype (format "-%s" filetype) ""))))
         (feature-name (intern (format "minemacs-first%s-file" (if filetype (format "-%s" filetype) ""))))
         (hook-docs (format "This hook will be run before opening the first %s file.
- Executed before `find-file-noselect'.
- It runs all hooks in `%s'.
- It provides the `%s' feature."
                            (or filetype "") hook-name feature-name)))
    `(progn
       (push ',hook-name +first-file-hooks)
       (+log! "Setting up hook `%s' and feature `%s'." ',hook-name ',feature-name)
       (defcustom ,hook-name nil ,hook-docs :group 'minemacs-core :type 'hook)
       (defun ,fn-name (&optional filename &rest _)
         (when (and
                after-init-time ; after Emacs initialization
                filename ; for named files
                (or
                 (featurep 'minemacs-loaded) ; after MinEmacs is loaded
                 (when-let* ((files (cdr command-line-args))) ; or immediately if the file is passed as a command line argument
                   (cl-some (lambda (file) (string= (expand-file-name filename) (expand-file-name file))) files)))
                (not (cl-some ; not an ignored file
                      (apply-partially #'string-prefix-p (expand-file-name filename))
                      (mapcar (+apply-partially-right #'file-expand-wildcards t)
                              (mapcar #'eval +first-file-hook-ignore-list))))
                (let ((case-fold-search t)) ; file name matches the regexp (case-insensitive)
                  (string-match-p ,ext-regexp filename)))
           (+log! "Running %d `%s' hooks (triggered by: %s)." (length ,hook-name) ',hook-name (abbreviate-file-name filename))
           (advice-remove 'find-file-noselect #',fn-name)
           (provide ',feature-name)
           (run-hooks ',hook-name)))
       (if (daemonp) ; load immediately after init when in daemon mode
           (add-hook 'after-init-hook (lambda () (provide ',feature-name) (run-hooks ',hook-name)) 90)
         (advice-add 'find-file-noselect :before #',fn-name '((depth . ,(if filetype -90 -91))))))))

;; From Doom Emacs
(defun +resolve-hook-forms (hooks)
  "Convert a list of modes into a list of hook symbols.

If a mode is quoted, it is left as is. If the entire HOOKS list is quoted, the
list is returned as-is."
  (declare (pure t) (side-effect-free t))
  (let ((hook-list (ensure-list (+unquote hooks))))
    (if (eq (car-safe hooks) 'quote)
        hook-list
      (cl-loop for hook in hook-list
               if (eq (car-safe hook) 'quote)
               collect (cadr hook)
               else collect (intern (format "%s-hook" (symbol-name hook)))))))

(defun +setq-hook-fns (hooks rest &optional singles advice-how)
  "HOOKS REST SINGLES ADVICE-HOW."
  (unless (or singles (= 0 (% (length rest) 2)))
    (signal 'wrong-number-of-arguments (list #'cl-evenp (length rest))))
  (cl-loop with vars = (let ((args rest)
                             vars)
                         (while args
                           (push (if singles
                                     (list (pop args))
                                   (cons (pop args) (pop args)))
                                 vars))
                         (nreverse vars))
           for hook-or-func in (if advice-how (ensure-list (+unquote hooks)) (+resolve-hook-forms hooks))
           append
           (cl-loop for (var . val) in vars
                    collect
                    (list var val hook-or-func
                          (intern
                           (format "+setq--%s%s-%s-%s"
                                   var (if advice-how advice-how "-in")
                                   hook-or-func (if advice-how "a" "h")))))))

;; From Doom Emacs
(defmacro +setq-hook! (hooks &rest var-vals)
  "Set buffer-local variables on HOOKS.

HOOKS can be expect receiving arguments (like in `enable-theme-functions'), the
`args' variable can be used inside VAR-VALS forms to get the arguments passed
the the function.

\(+setq-hook! \\='enable-theme-functions
  current-theme (car args))

\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (var val hook hook-fn) in (+setq-hook-fns hooks var-vals)
            collect `(defun ,hook-fn (&rest args)
                       ,(format "In `%S', locally set `%s' to %S." hook var val)
                       (setq-local ,var ,val))
            collect `(add-hook ',hook #',hook-fn -90))))

(defmacro +setq-advice! (funcs how &rest var-vals)
  "Set buffer-local variables as HOW advices for FUNCS.

FUNCS can be expect receiving arguments, the `args' variable can
be used inside VAR-VALS forms to get the arguments passed the the
function.

\(+setq-advice! #\\='revert-buffer :before
  revert-buffer-function #\\='ignore)

\(fn FUNCS HOW &rest [SYM VAL]...)"
  (declare (indent 2))
  (macroexp-progn
   (cl-loop for (var val func advice-fn) in (+setq-hook-fns funcs var-vals nil how)
            collect `(defun ,advice-fn (&rest args)
                       ,(format "Locally set `%s' to %S in `%S' `%S'." var val how func)
                       (setq-local ,var ,val))
            collect `(advice-add #',func ,how #',advice-fn))))

;; From Doom Emacs
(defmacro +unsetq-hook! (hooks &rest vars)
  "Unbind setq hooks on HOOKS for VARS.

\(fn HOOKS &rest VAR1 VAR2...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (_var _val hook fn)
            in (+setq-hook-fns hooks vars 'singles)
            collect `(remove-hook ',hook #',fn))))

;; https://emacs.stackexchange.com/a/82465/37002
(defun +fn-sans-advice (sym)
  "Get original function defined at SYM, sans advices."
  (if (advice--p (symbol-function sym))
      (advice--cd*r (symbol-function sym))
    (if (fboundp 'ad-get-orig-definition)
        (ad-get-orig-definition sym)
      sym)))

(defvar recentf-exclude)
(defun +ignore-root (&rest roots)
  "Add ROOTS to ignored projects, recentf, etc."
  (dolist (root roots)
    (with-eval-after-load 'recentf
      (add-to-list 'recentf-exclude (rx-to-string `(or ,root ,(expand-file-name root)))))))

(defun +package-disabled-p (package &optional module)
  "Is package PACKAGE disabled in `minemacs-disabled-packages'.

Optionally, check also for the containing MODULE."
  (or
   (and minemacs-builtin-only-p module (not (eq module 'me-builtin)))
   (and (memq package (flatten-list minemacs-disabled-packages)))
   (and module (not (memq module minemacs-modules)))))

(defun +package-configured-p (package)
  "Check if the PACKAGE has been configured by MinEmacs.
This is only valable after loading all modules (in the user's \"config.el\")."
  (and (memq package minemacs-configured-packages) t))

(defun minemacs-modules (&optional include-on-demand include-obsolete)
  "List all the available modules.
With optional INCLUDE-ON-DEMAND and INCLUDE-OBSOLETE."
  (let ((mod-files (directory-files minemacs-modules-dir nil "\\`me-.*\\.el\\'")))
    (when include-obsolete
      (cl-callf append mod-files (mapcar (apply-partially #'concat "obsolete/")
                                         (directory-files minemacs-obsolete-modules-dir nil "\\`me-.*\\.el\\'"))))
    (when include-on-demand
      (cl-callf append mod-files (mapcar (apply-partially #'concat "on-demand/")
                                         (directory-files minemacs-on-demand-modules-dir nil "\\`me-.*\\.el\\'"))))
    (mapcar #'intern (mapcar #'file-name-sans-extension mod-files))))

(defun minemacs-load-module (&rest modules)
  "Interactively install and load MODULES that aren't enabled in \"modules.el\".

When called with \\[universal-argument], it prompts also for on-demand modules.
When called with \\[universal-argument] \\[universal-argument], it prompts also for obsolete modules."
  (interactive (completing-read-multiple
                "Select modules: "
                (seq-filter (lambda (module) (not (featurep module)))
                            (let ((prefix (prefix-numeric-value current-prefix-arg)))
                              (minemacs-modules (>= prefix 4) (>= prefix 16))))))
  (let ((old-hooks ; save the old MinEmacs hooks to detect when the loaded module requires a hook to be run
         (append minemacs-after-startup-hook minemacs-lazy-hook
                 minemacs-after-load-theme-hook minemacs-after-setup-fonts-hook
                 (cl-loop for hook in +first-file-hooks append (eval hook))))
        (old-fns minemacs-build-functions-hook))
    (mapc #'+load (mapcar (apply-partially #'format "%s%s.el" minemacs-modules-dir) (flatten-list modules)))
    (let ((new-hooks
           (cl-set-difference
            (append minemacs-after-startup-hook minemacs-lazy-hook
                    minemacs-after-load-theme-hook minemacs-after-setup-fonts-hook
                    (cl-loop for hook in +first-file-hooks append (eval hook)))
            old-hooks))
          (minemacs-build-functions (cl-set-difference minemacs-build-functions old-fns)))
      (mapc #'funcall new-hooks)
      (minemacs-run-build-functions (not (interactive-p))))))



;;; Environment variables

(defvar +shell-command-switch
  (pcase shell-file-name
    ((rx bol "fish" eol) "-lc")
    ((rx bol (or "tsch" "csh") eol) "-dc")
    (_ "-ilc")))

;; Inspired by: https://emacs.stackexchange.com/a/21432/37002
(defun +shell-command-to-string-ignore-stderr (command)
  "Execute shell command COMMAND and return its output as a string.

Works like `shell-command-to-string' with three differences:
1. It uses `+shell-command-switch' instead of `shell-command-switch'.
2. It returns only stdout and ignore the output of stderr.
3. It sets TERM to \"smart\" instead of \"dumb\", to be able to escape from
Emacs-specific early exit in \".bashrc\"."
  (with-output-to-string
    (with-current-buffer standard-output
      ;; Do not pretend to be dumb (a.k.a. don't exit early when loading .bashrc)
      (let ((process-environment (append (list "TERM=smart") (remove "TERM=dumb" process-environment))))
        (process-file shell-file-name nil '(t nil) nil +shell-command-switch command)))))

(defun +env-save ()
  "Load environment variables from shell and save them to `+env-file'."
  (interactive)
  (unless (featurep 'os/win)
    (with-temp-buffer
      (insert ";; -*- mode: emacs-lisp; lexical-binding: t; no-byte-compile: t; no-native-compile: t; -*-\n\n"
              ";; This file is generated automatically via `+env-save'.\n\n")
      (let ((env-vars
             (mapcar ; Get environment variables from shell into an alist
              (lambda (line)
                (when-let* ((idx (string-search "=" line)))
                  (cons (substring line nil idx) (substring line (1+ idx)))))
              ;; "env --null" ends lines with null byte instead of newline
              (string-split (+shell-command-to-string-ignore-stderr "env --null") "\0" :omit-nulls))))
        ;; Special treatment for the "PATH" variable, save it to `exec-path'
        (when-let* ((path (alist-get "PATH" env-vars nil nil #'string=)))
          (insert "\n;; Adding PATH content to `exec-path'\n"
                  (format "(setq exec-path (delete-dups (append exec-path '%s)))\n\n"
                          (mapcar (apply-partially #'format "\"%s\"") (parse-colon-path path)))))
        ;; Save the environment variables to `process-environment' using `setenv'
        (insert ";; Adding the rest of the environment variables\n")
        (dolist (env-var env-vars)
          (unless (cl-some (+apply-partially-right #'string-match-p (car env-var)) +env-deny-vars)
            (let ((value (cdr env-var)))
              ;; Correctly handle special characters
              (dolist (pair '(("\a" . "\\a") ("\b" . "\\b") ("\f" . "\\f")
                              ("\n" . "\\n") ("\r" . "\\r") ("\t" . "\\t")
                              ("\v" . "\\v") ("\"" . "\\\"")))
                (setq value (string-replace (car pair) (cdr pair) value)))
              (insert (format "(setenv %S %S)\n" (car env-var) value))))))
      (write-file +env-file))))

(defun +env-load ()
  "Load environment variables from `+env-file'."
  (interactive)
  (unless (featurep 'os/win)
    (unless (file-exists-p +env-file) (+env-save))
    (+load +env-file)))



;;; Files, directories and IO helper functions

(defun +file-read-to-string (filename)
  "Return a string with the contents of FILENAME."
  (when (and (file-exists-p filename) (not (file-directory-p filename)))
    (with-temp-buffer
      (insert-file-contents filename)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun +directory-subdirs (dir)
  "Return a list of sub-directories in DIR."
  (when (and dir (file-directory-p dir))
    (mapcar #'abbreviate-file-name (seq-filter #'file-directory-p (directory-files dir t directory-files-no-dot-files-regexp)))))

(defun +directory-ensure (&rest path-parts)
  "Concatenate PATH-PARTS to construct a path and return it.

Ensure the path exists, if not create it. The exact behavior is to create the
parent directory if the path is a file, and if the path is a directory, create
that directory."
  (let* ((path (apply #'concat path-parts))
         (parent-dir (file-name-directory path)))
    (unless (file-directory-p parent-dir)
      (ignore-errors (mkdir parent-dir t))
      (unless (file-directory-p parent-dir)
        (+error! "Cannot create directory %s" parent-dir)))
    path))



;;; Lock files

(defun +lock--file (name)
  "Get the absolute path of the lockfile for resource NAME."
  (expand-file-name (format "minemacs-%s.lock" name) temporary-file-directory))

(defun +lock--locker-pid (name)
  "Get thecker PID of resource NAME."
  (let ((fname (+lock--file name)))
    (and (file-exists-p fname) (string-to-number (+file-read-to-string fname)))))

(defun +lockedp (name)
  "Return non-nil if the resource NAME is locked."
  (when-let* ((pid (+lock--locker-pid name)))
    (and (process-attributes pid) t)))

(defun +locked-by-current-process-p (name)
  "Return non-nil if the resource NAME locked by the current Emacs instance."
  (and (+lockedp name) (equal (emacs-pid) (+lock--locker-pid name))))

(defun +lock (name)
  "Lock the resource named NAME."
  (if (+lockedp name)
      (progn (+info! "Resource `%s' already locked!" name) nil)
    (+info! "Created lock file for resource `%s'!" name)
    (+shutup!
     (with-temp-buffer
       (insert (format "%d" (emacs-pid)))
       (write-file (+lock--file name))))
    t))

(defun +unlock (name &optional force-p)
  "Unlock the resource named NAME if locked by this process.
If FORCE-P is non-nil, force unlocking even if the resource is not locked by the
current process."
  (when (or force-p (+locked-by-current-process-p name))
    (+info! "Resource `%s' unlocked" name)
    (delete-file (+lock--file name))
    t))



;;; Misc Emacs tweaks

(defmacro +def-dedicated-tab! (cmd &rest body)
  "Define +CMD command to run BODY in a dedicated tab.
If not specified, BODY defaults to `(CMD)'.

You can pass an exit hook or exit function on which, the created workspace will
be deleted.

\(fn NAME [[:exit-hook HOOK] [:exit-func FUNC]] FORMS...)"
  (let* ((cmd (+unquote cmd))
         (fn-name (intern (format "+%s-dedicated-tab" cmd)))
         (fn-doc (format "Launch %s in a dedicated workspace." cmd))
         (tab-name (intern (format "+%s-tab-name" cmd)))
         (exit-fn-name (intern (format "+%s--close-workspace" cmd)))
         exit-func exit-hook sexp fn-body)
    (while (keywordp (car body))
      (pcase (pop body)
        (:exit-func (setq exit-func (+unquote (pop body))))
        (:exit-hook (setq exit-hook (+unquote (pop body))))))
    (setq sexp (if (null body) `((,cmd)) body))
    (when (or exit-func exit-hook)
      (setq
       fn-body
       `((defun ,exit-fn-name (&rest _)
           (when-let* ((tab-num (seq-position (tab-bar-tabs) ,tab-name (lambda (tab name) (string= name (alist-get 'name tab))))))
             (tab-close (1+ tab-num))))))
      (when exit-func
        (setq fn-body (append fn-body `((advice-add ',exit-func :after #',exit-fn-name)))))
      (when exit-hook
        (setq fn-body (append fn-body `((add-hook ',exit-hook #',exit-fn-name))))))
    `(progn
       (defvar ,tab-name ,(format "*%s*" cmd))
       (defun ,fn-name ()
         ,fn-doc
         (interactive)
         (when ,tab-name
           (tab-new)
           (tab-rename ,tab-name))
         (let (display-buffer-alist) ; To by pass the defined rules
           ,@sexp))
       ,(macroexp-progn fn-body)
       #',fn-name)))



;;; Eglot extras

;; Modified from Crafted Emacs, pass `eglot-server-programs' to this function
;; to fill `+eglot-auto-enable-modes' with all supported modes.
(defcustom +eglot-auto-enable-modes
  '(c-mode
    c++-mode c-ts-base-mode python-base-mode rust-mode
    rust-ts-mode cmake-mode cmake-ts-mode js-base-mode typescript-mode
    typescript-ts-base-mode json-mode json-ts-mode js-json-mode)
  "Modes for which Eglot can be automatically enabled by `+eglot-auto-enable'."
  :group 'minemacs-prog
  :type '(repeat symbol))

(defun +eglot--ensure-maybe-h ()
  "Maybe auto start Eglot if the current mode is in `+eglot-auto-enable-modes'."
  (when (derived-mode-p +eglot-auto-enable-modes) (eglot-ensure)))

(defun +eglot-auto-enable ()
  "Auto-enable Eglot in configured modes in `+eglot-auto-enable-modes'."
  (interactive)
  (add-hook 'after-change-major-mode-hook #'+eglot--ensure-maybe-h))

(defun +eglot-use-on-all-supported-modes (&optional mode-list)
  "Add all modes in MODE-LIST to `+eglot-auto-enable-modes'."
  (dolist (mode-def (or mode-list (bound-and-true-p eglot-server-programs)))
    (let ((mode (if (consp mode-def) (car mode-def) mode-def)))
      (cond
       ((consp mode) (+eglot-use-on-all-supported-modes mode))
       (t (unless (memq mode '(clojure-mode lisp-mode scheme-mode)) ; prefer cider, sly and geiser, respectively
            (add-to-list '+eglot-auto-enable-modes mode)))))))

(defvar eglot-server-programs)
(declare-function eglot-alternatives "eglot")
(defun +eglot-register (modes &rest servers)
  "Register MODES with LSP SERVERS.

Examples:
\(+eglot-register \\='vhdl-mode \"vhdl_ls\")
\(+eglot-register \\='lua-mode \"lua-language-server\" \"lua-lsp\")
\(+eglot-register \\='(c-mode c++-mode) \\='(\"clangd\" \"--clang-tidy\" \"-j=12\") \"ccls\")"
  (declare (indent 0))
  (with-eval-after-load 'eglot
    (let ((orig-val (assoc modes eglot-server-programs (lambda (s1 s2) (seq-intersection (ensure-list s1) (ensure-list s2)))))
          (contact (if (length> servers 1) (eglot-alternatives (ensure-list servers)) (if (functionp (car servers)) (car servers) (ensure-list (car servers))))))
      (if (null orig-val) ; not present, add it
          (add-to-list 'eglot-server-programs (cons modes contact))
        (unless (equal (car orig-val) modes) (setcar orig-val modes))
        (setcdr orig-val contact)))))



;;; Tramp

(defun +root-set-header ()
  "*Display a warning in header line of the current buffer.
This function is suitable to add to `find-file-hook' and `dired-file-hook'."
  (when-let* ((file (or buffer-file-name default-directory))
              ((string-equal (file-remote-p file 'user) "root")))
    (setq header-line-format (concat " "
                                     (+nerd-icons-icon "nf-md-pound_box" :face 'nerd-icons-red)
                                     (propertize " --- WARNING: EDITING FILE AS ROOT! %-" 'face 'error)))))

;;;###autoload
(define-minor-mode +sudo-indicator-mode
  "Indicates editing as root by displaying a message in the header line."
  :global t
  :lighter nil
  :group 'sudo-edit
  (if +sudo-indicator-mode
      (progn
        (add-hook 'find-file-hook #'+root-set-header)
        (add-hook 'dired-mode-hook #'+root-set-header))
    (remove-hook 'find-file-hook #'+root-set-header)
    (remove-hook 'dired-mode-hook #'+root-set-header)))



;;; Project tweaks

(defcustom +super-project-root-markers '(".super-project" ".super-project.el" ".repo" ".code-workspace" ".workspace")
  "List of super-project markers."
  :group 'minemacs-project
  :type '(repeat string))

(defun +project-super-project-try (dir)
  "Find super-project root starting from DIR."
  (when-let* ((root (cl-some (apply-partially #'locate-dominating-file dir) +super-project-root-markers)))
    (cons 'transient root)))

(defun +super-project-current (&optional dir)
  "Return the current super-project instance in DIR."
  (let ((project-find-functions '(+project-super-project-try)))
    (project-current nil dir)))

(defun +super-project-define-commands (package &rest commands)
  "Define PACKAGE's COMMANDS for super-project context."
  (declare (indent 1))
  (with-eval-after-load package
    (let (form)
      (dolist (command commands)
        (let ((new-cmd (intern (format "%s%s-super-project" (if (string-prefix-p "+" (symbol-name command)) "" "+") command))))
          (push
           `(defun ,new-cmd (&rest args)
              ,(format "Call `%s' in a super-project context." command)
              ,(interactive-form command) ; Use the same interactive form as the original command
              (if-let* ((project-find-functions '(+project-super-project-try))
                        ((project-current)))
                  (apply (function ,command) args)
                (user-error "It doesn't seem that we are in a super-project")))
           form)))
      (eval (macroexp-progn form)))))

(defun +project-safe-root (&optional proj)
  "Return the root of PROJ using several backends, don't fail."
  (when-let* ((root (if-let* ((proj (or proj (project-current))))
                        (project-root proj)
                      (vc-root-dir))))
    (expand-file-name root)))



;;; Proxy
;;; =====

(defun minemacs-get-enabled-proxies ()
  "Get a list of enabled proxies."
  (cl-loop
   for prox in '("no" "ftp" "http" "https")
   append (cl-loop for fn in '(downcase upcase)
                   collect (cons (funcall fn prox) (getenv (funcall fn (format "%s_proxy" prox)))))))

(defun minemacs-set-enabled-proxies (proxies)
  "Set PROXIES."
  (cl-loop
   for prox in proxies
   do (cl-loop
       for fn in '(upcase downcase)
       do (cons (funcall fn (car prox)) (setenv (funcall fn (format "%s_proxy" (car prox))) (cdr prox))))))

(defun minemacs-enable-proxy (proxies)
  "Set *_proxy Linux environment variables from PROXIES."
  (interactive (list minemacs-proxies))
  (minemacs-set-enabled-proxies proxies))

(defun minemacs-disable-proxy ()
  "Unset *_proxy Linux environment variables."
  (interactive)
  (minemacs-set-enabled-proxies (mapcar (lambda (a) (list (car a))) (minemacs-get-enabled-proxies))))

(defmacro +with-no-proxies! (&rest body)
  "Run BODY without proxies. Doesn't work with `emacs-async'.

Example:
  (+with-no-proxies! (async-shell-command \"git fetch --all\"))."
  `(let ((process-environment (cl-remove-if (lambda (env) (cl-some (lambda (prox) (string-prefix-p (format "%s_proxy=" (car prox)) env t)) (minemacs-get-enabled-proxies))) process-environment)))
     ,@body))



;;; Data serialization

(defcustom +serialized-symbols-directory (concat minemacs-local-dir "minemacs-serialized-symbols/")
  "Default directory to store serialized symbols."
  :group 'minemacs-core
  :type 'directory)

(defun +serialize-sym (sym &optional dir filename-format)
  "Serialize SYM to DIR.
If FILENAME-FORMAT is non-nil, use it to format the file name (ex.
\"file-%s.el\"). Return the written file name, or nil if SYM is not bound."
  (when (boundp sym)
    (let* ((out-file (expand-file-name
                      (format (or filename-format "%s.el") (symbol-name sym))
                      (or dir +serialized-symbols-directory)))
           (sym-val (eval sym))
           (read-sym-val (+deserialize-sym sym dir nil filename-format)))
      (unless (eq read-sym-val sym-val) ; no need to rewrite the file
        (mkdir +serialized-symbols-directory t)
        (+log! "Saving `%s' to file \"%s\"" (symbol-name sym) (abbreviate-file-name out-file))
        (with-temp-buffer
          (prin1 sym-val (current-buffer))
          (+shutup! (write-file out-file))))
      out-file)))

(defun +deserialize-sym (sym &optional dir mutate filename-format)
  "Deserialize SYM from DIR, if MUTATE is non-nil, assign the object to SYM.
If FILENAME-FORMAT is non-nil, use it to as a format (ex. \"file-%s.el\").
Return the deserialized object, or nil if the SYM.el
file dont exist."
  (let ((in-file (expand-file-name
                  (format (or filename-format "%s.el") (symbol-name sym))
                  (or dir +serialized-symbols-directory)))
        res)
    (when (file-exists-p in-file)
      (add-to-list '+first-file-hook-ignore-list in-file)
      (+log! "Loading `%s' from file \"%s\"" sym (abbreviate-file-name in-file))
      (with-temp-buffer
        (insert-file-contents in-file)
        (goto-char (point-min))
        (ignore-errors (setq res (read (current-buffer)))))
      (when mutate (set sym res)))
    res))



;;; Font and script settings

(defcustom minemacs-fonts-plist
  '(:default
    ((:family "Iosevka" :height 110)
     (:family "Martian Mono" :height 100)
     (:family "Cascadia Code" :height 110 :weight semi-light)
     (:family "Fira Code" :height 110)
     (:family "Geist Mono" :height 110)
     (:family "Jetbrains Mono" :height 100)
     (:family "Iosevka Comfy" :height 110)
     (:family "Iosevka Fixed Curly Slab" :height 110)
     (:family "Iosevka Fixed Curly" :height 110)
     (:family "Iosevka Comfy Fixed" :height 110)
     (:family "Iosevka Comfy Motion Fixed" :height 110)
     (:family "Hack" :height 110)
     (:family "Roboto Mono" :height 100)
     (:family "SF Mono" :height 110)
     (:family "Source Code Pro" :height 110)
     (:family "Menlo" :height 110)
     (:family "Monaco" :height 110)
     (:family "Ubuntu Mono" :height 110)
     (:family "DejaVu Sans Mono" :height 110)
     (:family "Consolas" :height 110))
    :fixed-pitch
    ((:inherit default))
    :fixed-pitch-serif
    ((:inherit default))
    :variable-pitch
    ((:family "Iosevka Comfy Motion Duo" :height 110)
     (:family "Geist" :height 110)
     (:family "Lato" :height 120)
     (:family "Roboto" :height 120)
     (:family "Inter" :height 120)
     "San Francisco"
     "Helvetica Neue"
     "Helvetica"
     "Ubuntu"
     "Liberation Sans"
     "Segoe UI")
    :symbol
    ((:family "Segoe UI Symbol" :prepend t)
     (:family "Symbola" :prepend t)
     (:family "Symbol" :prepend t))
    :emoji
    ((:family "Noto Color Emoji" :prepend t)
     (:family "Apple Color Emoji" :prepent t)
     (:family "Segoe UI Emoji" :prepend t)
     (:family "Quivira" :prepend t))
    ;; Arabic script
    :arabic
    ((:family "Cascadia Code" :height 120 :weight semi-light)
     "Amiri Typewriter"
     "KacstOne"
     "Greta Arabic"
     "Scheherazade"
     "Koodak"
     (:family "Amiri" :scale 0.9))
    ;; Chinese script
    :han
    ((:family "LXGW Neo Xihei" :scale 1.3)
     (:family "WenQuanYi Micro Hei Mono" :scale 1.3)
     (:family "LXGW WenKai Screen" :scale 1.3)
     (:family "LXGW WenKai Mono" :scale 1.3)
     (:family "PingFang SC" :scale 1.3)
     (:family "Microsoft Yahei UI" :scale 1.3)
     (:family "Simhei" :scale 1.3)))
  "MinEmacs fonts used by `+setup-fonts'.

The function checks and enables the first available font from these defined in
this plist. This variable can be customized to set font specs for specific Emacs
faces or to enable some language-specific fonts. The plist keywords are either
face names or script names expressed as keywords (with the \":\" prefix).

For example to set `default' face, use `:default', to set the `mode-line' face,
use `:mode-line', and so on. The parameters for each font in these cases (ie.
for face names) are used in the `custom-theme-set-faces' function, so you can
pass any specs (key value pairs) supported by `custom-theme-set-faces' (like
`:weight', `:slant', `:foreground', ...). A list of supported keywords are
available in the variable `+face-attributes'.

You can also setup some language-specific fonts. All scripts supported by Emacs
can be found in `+known-scripts'. The keyword in this plist will be the script
name expressed as a keyword, for example, for `latin' use `:latin', for `arabic'
use `:arabic', for `emoji' use `:emoji', and so on. In this case, the parameters
are used with `set-fontset-font', so you can send any key value pair supported
by `set-fontset-font'. A list of supported keywords in this case is available in
`+font-spec-keywords'.

The value of the extra `:prepend' keyword is passed the last argument to
`set-fontset-font'. The value of the extra `:scale' keyword can be used to set a
scaling factor for the font in Emacs' `face-font-rescale-alist'. See the
`+setup-fonts' implementation for more details."
  :group 'minemacs-ui
  :type 'plist)

(defconst +known-scripts (mapcar #'car script-representative-chars)
  "Supported scripts, like `latin', `arabic', `han', and so on.")

(defconst +face-attributes
  `( :family :foundry :width :height :weight :slant :foreground
     :distant-foreground :background :underline :overline :strike-through :box
     :inverse-video :stipple :font :inherit :extend)
  "Arguments accepted by the `set-face-attribute' function.")

(defconst +font-spec-keywords
  `( :family :foundry :width :weight :slant :adstyle :registry :dpi :size
     :spacing :avgwidth :name :script :lang :otf)
  "Arguments accepted by the `font-spec' function.")

(defun +font--get-valid-args (script-or-face font)
  "Get valid arguments from FONT for SCRIPT-OR-FACE."
  (if (stringp font)
      `(:family ,font)
    (apply
     #'append
     (mapcar (lambda (a) (list a (plist-get font a)))
             (cl-intersection
              (+plist-keys font)
              (if (memq script-or-face +known-scripts) +font-spec-keywords +face-attributes))))))

(defun +font-installed-p (font-family)
  "Check if FONT-FAMILY is installed on the system."
  (and font-family (member font-family (and (fboundp 'font-family-list) (font-family-list))) t))

(defun +apply-font-or-script (script-or-face)
  "Set font for SCRIPT-OR-FACE from `minemacs-fonts-plist'."
  (catch 'done
    (dolist (font (plist-get minemacs-fonts-plist (intern (format ":%s" script-or-face))))
      (let* ((spec (+font--get-valid-args script-or-face font))
             (scale (and (plistp font) (plist-get font :scale)))
             (prependp (and (plistp font) (plist-get font :prepend)))
             (family (plist-get spec :family))
             (scriptp (memq script-or-face +known-scripts)))
        (when (or (not family) (+font-installed-p family))
          (if scriptp
              (set-fontset-font t script-or-face (apply #'font-spec spec) nil prependp)
            (custom-theme-set-faces 'user `(,script-or-face ((t ,spec)))))
          (when (and scale family)
            (add-to-list 'face-font-rescale-alist (cons family scale)))
          (+log! "Setting %s `%s' to `%s'" (if scriptp "script" "face") script-or-face spec)
          (throw 'done spec))))))

;; Inspired by: https://github.com/seagle0128/.emacs.d/blob/master/custom-example.el
(defun +setup-fonts ()
  "Setup fonts."
  (interactive)
  (when (display-graphic-p)
    (mapc #'+apply-font-or-script
          (reverse
           (mapcar (lambda (k) (intern (substring (symbol-name k) 1)))
                   (+plist-keys minemacs-fonts-plist))))

    ;; Set the tooltip font accordingly
    (when-let* ((font (car (and (fboundp 'fontset-list) (fontset-list)))))
      (setq tooltip-frame-parameters (+alist-set 'font font tooltip-frame-parameters))))

  ;; Run hooks
  (run-hooks 'minemacs-after-setup-fonts-hook))

(add-hook 'window-setup-hook #'+setup-fonts)
(add-hook 'server-after-make-frame-hook #'+setup-fonts)

(defvar minemacs-reduce-font-size-ratio 0.82)
(defvar-local minemacs-reduce-font-remap-cookie nil)
(defun minemacs-reduce-font-size (&optional reset)
  "Reduce the font size in the buffer by `minemacs-reduce-font-size-ratio'.
When RESET is non-nil, restore the original font size."
  (interactive "P")
  (if reset
      (progn (face-remap-remove-relative minemacs-reduce-font-remap-cookie)
             (setq minemacs-reduce-font-remap-cookie nil))
    (when (and (display-graphic-p) (not minemacs-reduce-font-remap-cookie))
      (setq minemacs-reduce-font-remap-cookie (face-remap-add-relative 'default :height minemacs-reduce-font-size-ratio)))))

;; Reproduce the look of the subtle mode-line of the `spacious-padding' package
(defun +subtle-mode-line (&rest _args)
  "Subtle look for the mode-line."
  (when (display-graphic-p)
    (set-face-attribute
     'mode-line-active nil
     :box `(:line-width 4 :color ,(face-attribute 'default :background nil t) :style nil)
     :overline (face-attribute 'default :foreground nil t)
     :background (face-attribute 'default :background nil t))

    (set-face-attribute
     'mode-line-inactive nil
     :box `(:line-width 4 :color ,(face-attribute 'mode-line-inactive :background nil t) :style nil)
     :overline (face-attribute 'mode-line-inactive :foreground nil t))))

(defvar +tweak-faces t "Enable extra tweaks for faces, set to nil to disable it.")

(defun +tweak-faces (&optional _theme)
  (when +tweak-faces
    (set-face-attribute 'font-lock-builtin-face nil :weight 'medium :slant 'normal)
    (set-face-attribute 'font-lock-keyword-face nil :weight 'medium :slant 'italic)
    (set-face-attribute 'font-lock-type-face nil :weight 'semi-bold)
    (set-face-attribute 'font-lock-number-face nil :weight 'semi-bold)
    (set-face-attribute 'font-lock-function-name-face nil :weight 'medium :slant 'normal)
    (set-face-attribute 'font-lock-function-call-face nil :weight 'medium :slant 'normal)
    (with-eval-after-load 'eglot
      (set-face-attribute 'eglot-highlight-symbol-face nil :underline t))))

(add-hook 'enable-theme-functions #'+tweak-faces)

(autoload 'color-darken-name "color")
(autoload 'color-lighten-name "color")

(defun +color-subtle (base-color percentage &optional face-attr)
  "Make a more subtle color based on BASE-COLOR and PERCENTAGE.

We mean by subtle here, a darker color in dark themes and a lighter
color in light themes.

BASE-COLOR can be a color (string) or a face.
When it is a face, the FACE-ATTR needs to be provided, otherwise, the
:background attribute will be used."
  (let ((base-color (if (facep base-color)
                        (face-attribute base-color (or face-attr :background) nil t)
                      base-color)))
    (when (color-defined-p base-color)
      (funcall (if (eq 'light (frame-parameter nil 'background-mode)) #'color-lighten-name #'color-darken-name)
               base-color percentage))))

(defun +nerd-icons-icon (name &rest args)
  "Generic function to get icons by NAME, with ARGS."
  (when (featurep 'nerd-icons)
    (if-let* ((variant (and (string-match "^nf-\\([[:alnum:]]+\\)-" name) (match-string 1 name)))
              (fn (intern (format "nerd-icons-%sicon" variant)))
              ((fboundp fn)))
        (apply fn (cons name args))
      (error "Cannot detect the function which provides %S" name))))



;;; Emacs windows & buffers

(defun +make-buffer-conds (&rest conditions)
  "Return a lambda that matches CONDITIONS.
To be used as a predicate generator for `display-buffer-alist'."
  (lambda (buff-or-name &rest _args)
    (with-current-buffer (get-buffer buff-or-name)
      (cl-some
       (lambda (condition)
         (cond ((symbolp condition) (derived-mode-p condition))
               ((stringp condition) (string-match-p condition (buffer-name)))))
       conditions))))



;;; Lazy on-demand modules

(cl-defun minemacs-register-on-demand-module (module &optional &key auto-mode magic-mode magic-fallback-mode interpreter-mode companion-packages define-loader)
  "Register extra MODULE.

- :AUTO-MODE add an alist like `auto-mode-alist'.
- :MAGIC-MODE add an alist like `magic-mode-alist'.
- :MAGIC-FALLBACK-MODE add an alist like `magic-fallback-mode-alist'.
- :INTERPRETER-MODE add add an alist like
  `interpreter-mode-alist'.
- :COMPANION-PACKAGES defines companion packages for some modes like
- :DEFINE-LOADER defines a command `minemacs-load-<MODULE>' to manually load the
  module.
  \\='((some-mode . package))."
  (declare (indent 1))
  (unless (or minemacs-builtin-only-p (assq module minemacs-on-demand-modules-alist))
    (let ((plist nil))
      (when auto-mode
        (setq plist (append plist `(:auto-mode ,(ensure-list auto-mode)))))
      (when magic-mode
        (setq plist (append plist `(:magic-mode ,(ensure-list magic-mode)))))
      (when magic-fallback-mode
        (setq plist (append plist `(:magic-fallback-mode ,(ensure-list magic-mode)))))
      (when interpreter-mode
        (setq plist (append plist `(:interpreter-mode ,(ensure-list interpreter-mode)))))
      (when companion-packages
        (setq plist (append plist `(:companion-packages ,(ensure-list companion-packages)))))
      ;; We can pass a function or a form to `:define-loader'
      (when (cond ((functionp define-loader) (funcall define-loader))
                  (t (eval define-loader)))
        (let* ((mod-name (intern (format "on-demand/%s" module)))
               (cmd (intern (format "minemacs-load-%s" (string-remove-prefix "me-" (symbol-name module)))))
               (docstr (format "Load the `%s' module." mod-name)))
          (defalias cmd (lambda () (interactive) (require mod-name)) docstr)
          ;; Show the command only when the module isn't loaded
          (put cmd 'completion-predicate (lambda (_cmd _buf) (not (featurep mod-name))))))
      (push (cons module plist) minemacs-on-demand-modules-alist))))

(defun minemacs-on-demand-try ()
  "Loop over on-demand modules and load the ones available for the buffer."
  (let ((+use-package-check-for-disabled t)
        module-found)
    (dolist (module-spec minemacs-on-demand-modules-alist)
      (when-let* ((module (car module-spec))
                  (keys (+plist-keys (cdr module-spec))))
        (dolist (key keys)
          (when-let* ((enable (plist-get minemacs-on-demand-enable-plist key))
                      (key-specs (plist-get (cdr module-spec) key)))
            (unless (featurep (intern (format "on-demand/%s" module)))
              (dolist (mode-specs key-specs)
                (when-let* ((specs (ensure-list (car mode-specs)))
                            (modes (ensure-list (cdr mode-specs)))
                            ((cl-find-if-not #'fboundp modes))
                            ((pcase key
                               (:auto-mode
                                (and (buffer-file-name)
                                     (cl-find-if (+apply-partially-right #'string-match-p (buffer-file-name)) specs)))
                               (:interpreter-mode
                                (when-let* ((interpreter (save-excursion
                                                           (goto-char (point-min))
                                                           (when (looking-at auto-mode-interpreter-regexp)
                                                             (match-string 2)))))
                                  (member (file-name-nondirectory interpreter) specs)))
                               ((or :magic-mode :magic-fallback-mode)
                                (catch 'match-found
                                  (dolist (func-or-regexp specs)
                                    (when (or (and (functionp func-or-regexp)
                                                   (funcall func-or-regexp))
                                              (and (stringp func-or-regexp)
                                                   (save-excursion
                                                     (goto-char (point-min))
                                                     (save-restriction
                                                       (narrow-to-region (point-min) (min (point-max) (+ (point-min) magic-mode-regexp-match-limit)))
                                                       (let ((case-fold-search nil))
                                                         (looking-at func-or-regexp))))))
                                      (throw 'match-found t)))))))
                            ((or (eq enable 'no-ask)
                                 (and (not noninteractive) ; ask only when in an interactive session
                                      (y-or-n-p (format "Buffer %s can be opened with a mode from `%s', load it? "
                                                        (current-buffer) module))))))
                  (setq module-found t)
                  (+log! "Loading on-demand module %S" module)
                  (+load minemacs-on-demand-modules-dir (format "%s.el" module)))))))))
    (when module-found
      (set-auto-mode t) ; we set the mode automatically after loading the module
      (normal-mode)) ; this is needed, otherwise, it will only work on the second file
    nil)) ; return nil so the placeholder mode added to `magit-mode-alist' doesn't get applied

(defun minemacs-on-demand-try-load-companion-packages ()
  "Load companion packages for the current buffer's mode."
  (when-let* ((enable (plist-get minemacs-on-demand-enable-plist :companion-packages)))
    (let ((modules nil))
      (dolist (spec minemacs-on-demand-modules-alist)
        (let* ((module (car spec))
               (companion-packages (plist-get (cdr spec) :companion-packages)))
          (unless (featurep (intern (format "on-demand/%s" module)))
            (dolist (companion-assoc companion-packages)
              (let ((cur-modes (ensure-list (car companion-assoc)))
                    (modes (ensure-list (cdr companion-assoc))))
                (when-let* (((and (derived-mode-p cur-modes)
                                  (cl-find-if-not #'fboundp modes)
                                  (or (eq enable 'no-ask)
                                      (and (not noninteractive) ; ask only when in an interactive session
                                           (y-or-n-p (format "Module `%s' can be useful for buffer %s, load it? "
                                                             module (current-buffer))))))))
                  (push module modules)
                  (+load minemacs-on-demand-modules-dir (format "%s.el" module))))))))
      (when modules (set-auto-mode t))
      modules)))

(defun minemacs-load-companion-packages-for-buffer ()
  "Load companion packages applicables to the current's buffer mode."
  (interactive)
  (let ((minemacs-on-demand-enable-plist '(:companion-packages t)))
    (if-let* ((modules (minemacs-on-demand-try-load-companion-packages)))
        (message "Loaded on-demand modules %s." (string-join (mapcar (apply-partially #'format "`%s'") modules) ", "))
      (message "No suitable on-demand module for the current buffer."))))

;; We hook to `magic-mode-alist' and not `magit-fallback-mode-alist', this is
;; important for files that can be opened in some available mode but have a
;; dedicated on-demand mode. For example, opening a PKGBUILD file will activate
;; the `sh-mode', however, there is the `on-demand/me-pkgbuild' module which
;; contain the dedicated `pkgbuild-mode', so we need to load that. The
;; `fundamental-mode' here is just a placeholder, it won't be applied because
;; `minemacs-on-demand-try' returns nil.
(add-hook 'magic-mode-alist '(minemacs-on-demand-try . fundamental-mode))

(defun +prog-mode-run-hooks ()
  "Run the hooks in `prog-mode-hook'."
  (run-hooks 'prog-mode-hook))



(defun minemacs-generate-loaddefs ()
  "Generate MinEmacs' loaddefs file."
  (interactive)
  (when (file-exists-p minemacs-loaddefs-file) (delete-file minemacs-loaddefs-file))
  (loaddefs-generate
   (list minemacs-core-dir
         (concat minemacs-core-dir "extras/")
         minemacs-elisp-dir
         (concat minemacs-modules-dir "extras/")
         minemacs-on-demand-modules-dir)
   minemacs-loaddefs-file))

;; Functions
(defun +load-user-configs (&rest configs)
  "Load user configurations CONFIGS."
  (dolist (conf configs)
    (unless (memq conf minemacs-ignore-user-config)
      (let ((conf-path (format "%s%s.el" minemacs-config-dir conf)))
        (when (file-exists-p conf-path) (+load conf-path))))))

(defun +load (&rest filename-parts)
  "Load a file, the FILENAME-PARTS are concatenated to form the file name."
  (let ((filename (file-truename (apply #'file-name-concat filename-parts))))
    (if (file-exists-p filename)
        (with-demoted-errors "[MinEmacs:LoadError] %s"
          (load filename nil (not minemacs-verbose-p)))
      (+log! "Cannot load \"%s\", the file doesn't exists." filename))))


(provide 'me-lib)
;;; me-lib.el ends here
