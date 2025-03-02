;; me-lib.el -- MinEmacs Library (helper functions, extra features and commands) -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(require 'me-vars)
(require 'cl-lib)
(require 'rx)

(autoload 'cl-loop "cl-macs" nil nil 'macro)
(autoload 'url-filename "url-parse")
(autoload 'url-generic-parse-url "url-parse")
(autoload 'vc-git-root "vc-git")
(autoload 'vc-git-revert "vc-git")
(autoload 'tramp-make-tramp-file-name "tramp")
(defvar tramp-root-id-string) ; Make byte-compiler happy



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

(defmacro +plist-push! (plist &rest key-vals)
  "Push KEY-VALS to PLIST."
  (declare (indent 1))
  (let ((out (list 'progn)))
    (while (length> key-vals 0)
      (let ((key (pop key-vals))
            (val (pop key-vals)))
        (cl-callf append out `((setq ,plist (plist-put ,plist ,key ,val))))))
    out))

(defun +plist-combine (&rest plists)
  "Create a single property list from all plists in PLISTS.
Modified from `org-combine-plists'. This supposes the values to be vectors,
and concatenate them."
  (let ((res (copy-sequence (pop plists)))
        prop val plist)
    (while plists
      (setq plist (pop plists))
      (while plist
        (setq prop (pop plist) val (pop plist))
        (setq res (plist-put res prop (vconcat val (plist-get res prop))))))
    res))

(defun +plist-delete (plist prop)
  "Delete property PROP from PLIST.
Adapted from `org-plist-delete'."
  (let (p)
    (while plist
      (if (not (eq prop (car plist)))
          (setq p (plist-put p (car plist) (nth 1 plist))))
      (setq plist (cddr plist)))
    p))

(defun +plist-to-alist (plist &optional trim-col)
  "Convert PLIST to an alist, trim first colon when TRIM-COL."
  (let (res)
    (while plist
      (let* ((key (pop plist))
             (val (pop plist))
             (key (if (and trim-col (string-prefix-p ":" (symbol-name key)))
                      (intern (substring (symbol-name key) 1))
                    key)))
        (push (cons key val) res)))
    (nreverse res)))

(defun +alist-to-plist (alist &optional add-col)
  "Convert ALIST to a plist, add colon to the keys when ADD-COL."
  (let (res)
    (dolist (x alist)
      (let ((key (car x)))
        (cl-assert (or (and (not nil) (atom key)) (stringp key)) t "The alist should have keys that are symbols or strings")
        (push (if add-col (intern (format ":%s" key)) key) res)
        (push (cdr x) res)))
    (nreverse res)))

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

(defmacro +mode-alist-add-ts-modes! (mode-alist)
  "Duplicate elements in MODE-ALIST to include Treesit modes.

For the alist \=((some-mode . spec)), this will add \=(some-ts-mode . spec)."
  `(cl-callf append ,mode-alist
     (cl-loop
      for mode-spec in ,mode-alist
      collect (let ((ts-mode (intern (format "%s-ts-mode" (string-remove-suffix "-mode" (symbol-name (car mode-spec)))))))
                (when (fboundp ts-mode) (cons ts-mode (cdr mode-spec)))))))

;;; Missing primitive utilities

;; Adapted from `evil-unquote', takes functions into account
(defun +unquote (expr)
  "Return EXPR unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe expr) '(quote function))
    (setq expr (cadr expr)))
  expr)

(defun +quoted-p (expr)
  "Return t when EXPR is quoted."
  (memq (car-safe expr) '(quote function)))

(defun +apply-partially-right (fun &rest args)
  "Like `apply-partially', but apply the ARGS to the right of FUN."
  (lambda (&rest args2)
    (apply fun (append args2 args))))



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
  (let ((message-log-max (and minemacs-verbose-p message-log-max)))
    (with-temp-message (or (current-message) "")
      (+debug! "Inhibiting messages of %s" (symbol-name fn))
      (apply fn args))))

(defun +load-theme ()
  "Load Emacs' theme from `minemacs-theme'."
  (interactive)
  (when minemacs-theme
    (+log! "Loading user theme: %s" minemacs-theme)
    ;; Fallback to built-in `tsdh-light' when `minemacs-theme' is not available.
    (unless (ignore-errors (load-theme minemacs-theme t))
      (let ((default-theme (eval (car (get 'minemacs-theme 'standard-value)))))
        (+error! "Cannot load theme %S, trying to load the default theme %S" minemacs-theme default-theme)
        (unless (ignore-errors (load-theme default-theme t))
          (+error! "Cannot load default theme %S, falling back to the builtin `modus-operandi' theme" default-theme)
          (load-theme 'modus-operandi t)))))
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
  "A list of files to ignore in the `minemacs-first-*-file-hook'."
  :group 'minemacs-core
  :type '(repeat (choice file sexp)))

(defcustom +first-file-hooks nil
  "A list of defined hooks using `+make-first-file-hook!'."
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
       (+log! "Setting up hook `%s' -- function `%s' -- feature `%s'." ',hook-name ',fn-name ',feature-name)
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
           (+log! "Running %d `%s' hooks (triggered by: %s)." (length ,hook-name) ',hook-name filename)
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

  (+setq-hook! \\='enable-theme-functions
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

  (+setq-advice! #\\='revert-buffer :before
    `revert-buffer-function' #\\='ignore)

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

;; Taken from: https://sachachua.com/dotemacs/index.html#building-a-today-i-learned-habit-and-displaying-the-documentation-for-random-emacs-commands
(defun +describe-random-command ()
  "Show the documentation for a random command.
Consider only documented, non-obsolete interactive functions."
  (interactive)
  (let (result)
    (mapatoms
     (lambda (s)
       (when (and (commandp s)
                  (documentation s t)
                  (not (get s 'byte-obsolete-info)))
         (setq result (cons s result)))))
    (funcall-interactively (if (fboundp 'helpful-command) 'helpful-command 'describe-command)
                           (elt result (random (length result))))))



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
  (unless (+emacs-options-p 'os/win)
    (with-temp-buffer
      (insert ";; -*- mode: emacs-lisp; no-byte-compile: t; no-native-compile: t; -*-\n\n")
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
  (unless (+emacs-options-p 'os/win)
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
         (fn-name (intern (format "+%s" cmd)))
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
           (if (fboundp 'tabspaces-mode)
               ;; When `tabspaces' is available, use it.
               (when-let* ((tab-num (seq-position (tabspaces--list-tabspaces) ,tab-name #'string=)))
                 (tabspaces-close-workspace (1+ tab-num)))
             ;; Or default to the built-in `tab-bar'.
             (when-let* ((tab-num (seq-position (tab-bar-tabs) ,tab-name (lambda (tab name) (string= name (alist-get 'name tab))))))
               (tab-close (1+ tab-num)))))))
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
           (if (fboundp 'tabspaces-mode)
               (tabspaces-switch-or-create-workspace ,tab-name)
             (tab-new)
             (tab-rename ,tab-name)))
         ,@sexp)
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
  (+eglot-register \\='vhdl-mode \"vhdl_ls\")
  (+eglot-register \\='lua-mode \"lua-language-server\" \"lua-lsp\")
  (+eglot-register \\='(c-mode c++-mode) \\='(\"clangd\" \"--clang-tidy\" \"-j=12\") \"ccls\")"
  (declare (indent 0))
  (with-eval-after-load 'eglot
    (let ((orig-val (assoc modes eglot-server-programs (lambda (s1 s2) (seq-intersection (ensure-list s1) (ensure-list s2)))))
          (contact (if (length> servers 1) (eglot-alternatives (ensure-list servers)) (ensure-list (car servers)))))
      (if (null orig-val) ; not present, add it
          (add-to-list 'eglot-server-programs (cons modes contact))
        (unless (equal (car orig-val) modes)
          (setcar orig-val modes))
        (setcdr orig-val contact)))))



;;; Binary files tweaks

;; (add-to-list 'magic-fallback-mode-alist '(+binary-hexl-buffer-p . +binary-hexl-mode-maybe) t)

(defcustom +binary-hexl-enable t
  "Enable or disable opening suitable files in `hexl-mode'."
  :group 'minemacs-binary
  :type 'boolean)



;;; Project tweaks

(defcustom +project-scan-dir-paths nil
  "A list of paths to scan and add to known projects list.
It can be a list of strings (paths) or a list of (\"~/path\" .
recursive-p) to scan directories recursively."
  :group 'minemacs-project
  :type '(repeat (choice directory (cons directory boolean))))

(declare-function project-remember-projects-under "project")

(defun +project-scan-for-projects (&rest dirs)
  "Scan and remember projects under DIRS or `+project-scan-dir-paths'."
  (interactive)
  (dolist (cons-dir (or dirs +project-scan-dir-paths))
    (let* ((cons-dir (ensure-list cons-dir))
           (root-dir (car cons-dir))
           (recursive (cdr cons-dir))
           (sub-dirs (and (file-directory-p root-dir) (+directory-subdirs root-dir))))
      (dolist (dir sub-dirs)
        (project-remember-projects-under dir recursive)))))

(defcustom +super-project-root-markers '(".super-project" ".super-project.el" ".repo" ".code-workspace" ".workspace")
  "List of super-project markers."
  :group 'minemacs-project
  :type '(repeat string))

(defun +project-super-project-try-or-fail (dir)
  "Find super-project root starting from DIR."
  (if-let* ((root (cl-some (apply-partially #'locate-dominating-file dir) +super-project-root-markers)))
      (cons 'local root)
    (user-error "It doesn't seem that we are in a super-project")))

(defun +super-project-define-commands (package &rest commands)
  "Define PACKAGE's COMMANDS for super-project context."
  (declare (indent 1))
  (with-eval-after-load package
    (let (form)
      (dolist (command commands)
        (let ((new-cmd (intern (format "%s%s-super-project" (if (string-prefix-p "+" (format "%s" command)) "" "+") command))))
          (push
           `(defun ,new-cmd (&rest args)
              ,(format "Call `%s' in a super-project context." command)
              ,(interactive-form command) ; Use the same interactive form as the original command
              (let ((project-find-functions '(+project-super-project-try-or-fail)))
                (apply (function ,command) args)))
           form)))
      (eval (macroexp-progn form)))))

(declare-function project-root "project")
(declare-function projectile-project-root "projectile")
(declare-function ffip-project-root "find-file-in-project")

(defun +project-safe-root (&optional proj)
  "Return the root of PROJ using several backends, don't fail."
  (let ((proj (or proj (project-current))))
    (or
     (and proj (project-root proj))
     (and (fboundp 'ffip-project-root) (ffip-project-root))
     (and (fboundp 'projectile-project-p) (projectile-project-p) (projectile-project-root))
     (vc-root-dir))))



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
    ((:family "Iosevka Fixed Curly Slab" :height 110)
     (:family "Iosevka Fixed Curly" :height 110)
     (:family "Iosevka Comfy Fixed" :height 110)
     (:family "Iosevka Comfy Motion Fixed" :height 110)
     (:family "Iosevka" :height 110)
     (:family "Iosevka Comfy" :height 110)
     (:family "Martian Mono" :height 100)
     (:family "Cascadia Code" :height 110)
     (:family "Fira Code" :height 110)
     (:family "Jetbrains Mono" :height 100)
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
    ("Amiri Typewriter"
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
  (list :family :foundry :width :height :weight :slant :foreground
        :distant-foreground :background :underline :overline :strike-through :box
        :inverse-video :stipple :font :inherit :extend)
  "Arguments accepted by the `set-face-attribute' function.")

(defconst +font-spec-keywords
  (list :family :foundry :width :weight :slant :adstyle :registry :dpi :size
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

;; Reproduce the look of the subtle mode-line of the `spacious-padding' package
(defun +subtle-mode-line ()
  "Subtle look for the mode-line."
  (set-face-attribute
   'mode-line-active nil
   :overline (face-attribute 'default :foreground nil t)
   :background (face-attribute 'default :background nil t)))

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



;;; Lazy on-demand modules

(cl-defun minemacs-register-on-demand-module (module &optional &key auto-mode magic-mode interpreter-mode companion-packages)
  "Register extra MODULE.

- :AUTO-MODE add an alist like `auto-mode-alist'.
- :MAGIC-MODE add an alist like `magic-mode-alist'.
- :INTERPRETER-MODE add add an alist like
  `interpreter-mode-alist'.
- :COMPANION-PACKAGES defines companion packages for some modes like
  \\='((some-mode . package))."
  (declare (indent 1))
  (unless (or minemacs-builtin-only-p (assq module minemacs-on-demand-modules-alist))
    (let ((plist nil))
      (when auto-mode
        (setq plist (append plist `(:auto-mode ,(ensure-list auto-mode)))))
      (when magic-mode
        (setq plist (append plist `(:magic-mode ,(ensure-list magic-mode)))))
      (when interpreter-mode
        (setq plist (append plist `(:interpreter-mode ,(ensure-list interpreter-mode)))))
      (when companion-packages
        (setq plist (append plist `(:companion-packages ,(ensure-list companion-packages)))))
      (push (cons module plist) minemacs-on-demand-modules-alist))))

;; Ensure `minemacs-try-load-extra-mode' is the last to be evaluated
(add-hook 'magic-mode-alist '(minemacs-try-load-extra-mode . fundamental-mode) 100)

(defun minemacs-try-load-extra-mode ()
  "Load extra mode if available."
  (prog1 nil ; always return nil to avoid applying the `fundamental-mode'
    (or (minemacs-on-demand-try-auto-mode)
        (minemacs-on-demand-try-interpreter-mode)
        (minemacs-on-demand-try-magic-mode))))

(add-hook 'after-change-major-mode-hook #'minemacs-on-demand-try-load-companion-packages 95)

(defun minemacs-on-demand-try-load-companion-packages ()
  "Load companion packages for the current buffer's mode."
  (let ((mods nil))
    (when minemacs-on-demand-enable-companion-packages
      (dolist (spec minemacs-on-demand-modules-alist)
        (let* ((module (car spec))
               (plist (cdr spec))
               (companion-packages (plist-get plist :companion-packages)))
          (unless (featurep (intern (format "on-demand/%s" module)))
            (dolist (companion-assoc companion-packages)
              (let ((cur-modes (ensure-list (car companion-assoc)))
                    (modes (ensure-list (cdr companion-assoc))))
                (when-let* (((and (apply #'derived-mode-p cur-modes)
                                  (cl-find-if-not #'fboundp modes)
                                  (or (eq minemacs-on-demand-enable-companion-packages 'no-ask)
                                      (and (not noninteractive) ; ask only when in an interactive session
                                           (y-or-n-p (format "Module `%s' can be useful for buffer %s, load it? "
                                                             module (current-buffer))))))))
                  (push module mods)
                  (+load minemacs-on-demand-modules-dir (format "%s.el" module)))))))))
    (when mods (set-auto-mode t))
    mods))

(defun minemacs-load-companion-packages-for-buffer ()
  "Load companion packages applicables to the current's buffer mode."
  (interactive)
  (let ((minemacs-on-demand-enable-companion-packages t))
    (if-let* ((mods (minemacs-on-demand-try-load-companion-packages)))
        (message "Loaded on-demand modules %s." (string-join (mapcar (apply-partially #'format "`%s'") mods) ", "))
      (message "No suitable on-demand module for the current buffer."))))

(defun minemacs-on-demand-try-auto-mode ()
  "Try to automatically enable a mode for the current buffer."
  (let ((mods nil))
    (when minemacs-on-demand-enable-auto-mode
      (dolist (spec minemacs-on-demand-modules-alist)
        (let* ((module (car spec))
               (auto-modes (plist-get (cdr spec) :auto-mode)))
          (unless (featurep (intern (format "on-demand/%s" module)))
            (dolist (auto-mode auto-modes)
              (let ((regexps (ensure-list (car auto-mode)))
                    (mode (cdr auto-mode)))
                (when-let* (((and (buffer-file-name)
                                  (cl-find-if (lambda (regexp) (string-match regexp (buffer-file-name))) regexps)
                                  (not (fboundp mode))
                                  (or (eq minemacs-on-demand-enable-auto-mode 'no-ask)
                                      (and (not noninteractive) ; ask only when in an interactive session
                                           (y-or-n-p (format "File %s can be opened with `%s' from `%s', load it? "
                                                             (abbreviate-file-name (buffer-file-name)) mode module)))))))
                  (push module mods)
                  (+load minemacs-on-demand-modules-dir (format "%s.el" module)))))))))
    (when mods (set-auto-mode t))
    mods))

(defun minemacs-on-demand-try-magic-mode ()
  "Try to automatically enable a mode for FILENAME."
  (let ((mods nil))
    (when minemacs-on-demand-enable-magic-mode
      (dolist (spec minemacs-on-demand-modules-alist)
        (let* ((module (car spec))
               (magic-modes (plist-get (cdr spec) :magic-mode)))
          (unless (featurep (intern (format "on-demand/%s" module)))
            (dolist (magic-mode magic-modes)
              (let ((func-or-regexp (car magic-mode))
                    (mode (cdr magic-mode)))
                (when-let* (((and (not (fboundp mode))
                                  (cond ((functionp func-or-regexp) (funcall func-or-regexp))
                                        ((stringp func-or-regexp)
                                         (save-excursion
                                           (goto-char (point-min))
                                           (save-restriction
                                             (narrow-to-region (point-min) (min (point-max) (+ (point-min) magic-mode-regexp-match-limit)))
                                             (let ((case-fold-search nil))
                                               (looking-at func-or-regexp))))))
                                  (or (eq minemacs-on-demand-enable-magic-mode 'no-ask)
                                      (and (not noninteractive) ; ask only when in an interactive session
                                           (y-or-n-p (format "Buffer %s can be opened with `%s' from `%s', load it? "
                                                             (current-buffer) mode module)))))))
                  (push module mods)
                  (+load minemacs-on-demand-modules-dir (format "%s.el" module)))))))))
    (when mods (set-auto-mode t))
    mods))

(defun minemacs-on-demand-try-interpreter-mode ()
  "Try to automatically enable a mode based on the `:interpreter-mode' value."
  (let ((mods nil))
    (when minemacs-on-demand-enable-interpreter-mode
      (dolist (spec minemacs-on-demand-modules-alist)
        (let* ((module (car spec))
               (interpreter-modes (plist-get (cdr spec) :interpreter-mode)))
          (unless (featurep (intern (format "on-demand/%s" module)))
            (dolist (interpreter-mode interpreter-modes)
              (let ((interpreter (car interpreter-mode))
                    (mode (cdr interpreter-mode)))
                (when-let* (((and (not (fboundp mode))
                                  (when-let* ((interp (save-excursion
                                                        (goto-char (point-min))
                                                        (when (looking-at auto-mode-interpreter-regexp)
                                                          (match-string 2)))))
                                    (string-match-p (format "\\`%s\\'" interpreter) (file-name-nondirectory interp)))
                                  (or (eq minemacs-on-demand-enable-interpreter-mode 'no-ask)
                                      (and (not noninteractive) ; ask only when in an interactive session
                                           (y-or-n-p (format "Buffer %s can be opened with `%s' from `%s', load it? "
                                                             (current-buffer) mode module)))))))
                  (push module mods)
                  (+load minemacs-on-demand-modules-dir (format "%s.el" module)))))))))
    (when mods (set-auto-mode t))
    mods))

(defun +prog-mode-run-hooks ()
  "Run the hooks in `prog-mode-hook'."
  (run-hooks 'prog-mode-hook))



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
      (message "[MinEmacs:Error] Cannot load \"%s\", the file doesn't exists." filename))))

(defun +emacs-options-p (&rest feats)
  "Is features FEATS are enabled in this Emacs build.
When the first argument is `:any', this returns t if at least one of the
FEATS is available."
  (let ((fn (if (eq (car feats) :any) (progn (setq feats (cdr feats)) #'cl-some) #'cl-every)))
    (and (funcall fn (lambda (feat) (memq feat minemacs--options)) feats) t)))


(provide 'me-lib)
;;; me-lib.el ends here
