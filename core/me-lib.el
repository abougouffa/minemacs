;; me-lib.el -- MinEmacs Library (helper functions, extra features and commands) -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(require 'me-vars)

(autoload 'cl-loop "cl-macs" nil nil 'macro)
(autoload 'url-filename "url-parse")
(autoload 'url-generic-parse-url "url-parse")
(autoload 'vc-git-root "vc-git")
(autoload 'vc-git-revert "vc-git")
(autoload 'tramp-make-tramp-file-name "tramp")
(defvar tramp-root-id-string) ; Make byte-compiler happy

(require 'rx)



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
        (setq out (append out `((setq ,plist (plist-put ,plist ,key ,val)))))))
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
      (push (if add-col (intern (format ":%s" (car x))) (car x)) res)
      (push (cdr x) res))
    (nreverse res)))

;; Taken from: emacs.stackexchange.com/q/33892/12534
(defun +alist-set (key val alist &optional symbol)
  "Set property KEY to VAL in ALIST. Return new alist.
This creates the association if it is missing, and otherwise sets the cdr of the
first matching association in the list. It does not create duplicate
associations. By default, key comparison is done with `equal'. However, if
SYMBOL is non-nil, then `eq' is used instead.

This method may mutate the original alist, but you still need to use the return
value of this method instead of the original alist, to ensure correct results."
  (if-let ((pair (if symbol (assq key alist) (assoc key alist))))
      (setcdr pair val)
    (push (cons key val) alist))
  alist)

;;; Missing primitive utilities

(defun +set-standard-value (variable value)
  "Set the standard value of VARIABLE to VALUE."
  (put variable 'standard-value `((funcall (function ,(lambda nil "" value))))))

(defun +standard-value (variable)
  "Return the standard value for VARIABLE."
  (eval (car (get variable 'standard-value)) t))

(defun +reset-sym (sym)
  "Reset SYM to its standard value."
  (set sym (+standard-value sym)))

(defmacro +reset-var! (var)
  "Reset VAR to its standard value."
  `(setq ,var (+standard-value ',var)))

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

(defun +emacs-features-p (&rest feats)
  "Is features FEATS are enabled in this Emacs build."
  (and (cl-every (lambda (feat) (memq feat emacs/features)) feats) t))

(defmacro +fn-inhibit-messages! (fn &optional no-message-log)
  "Add an advice around the function FN to suppress messages in echo area.
If NO-MESSAGE-LOG is non-nil, do not print any message to *Messages* buffer."
  (let ((advice-fn (make-symbol (format "+%s--inhibit-messages:around-a" fn))))
    `(advice-add
      ',fn :around
      (defun ,advice-fn (origfn &rest args)
       (let ((message-log-max (unless ,no-message-log message-log-max)))
        (with-temp-message (or (current-message) "")
         (+log! "Inhibiting messages of %s" ,(symbol-name fn))
         (apply origfn args)))))))

(defmacro +shutup! (&rest body)
  "Suppress new messages temporarily while evaluating BODY.
This inhebits both the echo area and the `*Messages*' buffer."
  (if (not minemacs-verbose-p)
      `(let ((message-log-max nil))
        (with-temp-message (or (current-message) "") ,@body))
    `(progn ,@body)))

(defmacro +cmdfy! (&rest body)
  "Convert BODY to an interactive command."
  `(lambda () (interactive) ,@body))

(defun +load-theme ()
  "Load Emacs' theme from `minemacs-theme'."
  (interactive)
  (when minemacs-theme
    (+log! "Loading user theme: %s" minemacs-theme)
    ;; Fallback to built-in `tsdh-light' when `minemacs-theme' is not available.
    (unless (ignore-errors (load-theme minemacs-theme t))
      (let ((default-theme (+standard-value 'minemacs-theme)))
        (+error! "Cannot load theme %S, trying to load the default theme %S" minemacs-theme default-theme)
        (unless (ignore-errors (load-theme default-theme t))
          (+error! "Cannot load default theme %S, falling back to the builtin tsdh-light theme" default-theme)
          (load-theme 'tsdh-light t)))))
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
         (when-let (fn (pop fns))
           (+log! "Running task %d, calling function `%s'" task-num (truncate-string-to-width (format "%s" fn) 40 nil nil "…"))
           (funcall fn))
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

(defmacro +deferred-when! (condition &rest body)
  "Like `+deferred!', with BODY executed only if CONDITION is non-nil."
  (declare (indent 1))
  `(when ,condition (+deferred! ,@body)))

(defmacro +deferred-unless! (condition &rest body)
  "Like `+deferred!', with BODY executed only if CONDITION is nil."
  (declare (indent 1))
  `(unless ,condition (+deferred! ,@body)))

(defmacro +lazy! (&rest body)
  "Run BODY as a lazy block (see `minemacs-lazy')."
  `(with-eval-after-load 'minemacs-lazy
    (+eval-when-idle-for! +lazy-delay ,@body)))

(defmacro +lazy-when! (condition &rest body)
  "Like `+lazy!', with BODY executed only if CONDITION is non-nil."
  (declare (indent 1))
  `(when ,condition (+lazy! ,@body)))

(defmacro +lazy-unless! (condition &rest body)
  "Like `+lazy!', with BODY executed only if CONDITION is nil."
  (declare (indent 1))
  `(unless ,condition (+lazy! ,@body)))

(defmacro +after-load! (features &rest body)
  "Execute BODY after FEATURES have been loaded."
  (declare (indent 1))
  (let ((features (if (+quoted-p features) (+unquote features) (eval features))))
    (if (symbolp features)
        `(with-eval-after-load ',features ,@body)
      (let ((feature (car features)))
        (cond
         ((memq feature '(:or :any))
          (macroexp-progn
           (cl-loop for next in (cdr features)
                    collect `(with-eval-after-load ',(+unquote next) ,@body))))
         ((memq feature '(:and :all))
          (dolist (next (reverse (cdr features)) (car body))
            (setq body `((with-eval-after-load ',(+unquote next) ,@body)))))
         (t `(+after-load! '(:all ,@features) ,@body)))))))

;; Adapted from: github.com/d12frosted/environment
(defmacro +hook-with-delay! (hook secs function &optional depth local)
  "Add the FUNCTION to the value of HOOK.
The FUNCTION is delayed to be evaluated in SECS once HOOK is
triggered.
DEPTH and LOCAL are passed as is to `add-hook'."
  (let* ((f-name (make-symbol (format "+%s-on-%s-delayed-%.2fs-h" (+unquote function) (+unquote hook) secs)))
         (f-doc (format "Call `%s' in %d seconds" (symbol-name (+unquote function)) secs)))
    `(eval-when-compile
       (defun ,f-name () ,f-doc
        (run-with-idle-timer ,secs nil ,function))
       (add-hook ,hook #',f-name ,depth ,local))))

(defvar +hook-once-num 0)

(defmacro +hook-once! (hook &rest body)
  "Hook BODY in HOOK, and make it run only once."
  (declare (indent 1))
  (let ((hook (+unquote hook))
        (fn-name (intern (format "+hook-once--function-%d-h" (cl-incf +hook-once-num)))))
    `(add-hook ',hook
      (defun ,fn-name (&rest _)
       ,(macroexp-progn body)
       (remove-hook ',hook ',fn-name)))))

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

Applies to files that matches %S.

Executed before `find-file-noselect', it runs all hooks in `%s' and provide the `%s' feature."
                            (or filetype "") (eval ext-regexp) hook-name feature-name)))
    `(progn
       (+log! "Setting up hook `%s' -- function `%s' -- feature `%s'." ',hook-name ',fn-name ',feature-name)
       (defcustom ,hook-name nil ,hook-docs :group 'minemacs-core :type 'hook)
       (defun ,fn-name (&optional filename &rest _)
        (when (and
               after-init-time ; after Emacs initialization
               (or
                (featurep 'minemacs-loaded) ; after MinEmacs is loaded
                (when-let ((files (cdr command-line-args))) ; or immediately if the file is passed as a command line argument
                 (cl-some (lambda (file) (string= (expand-file-name filename) (expand-file-name file))) files)))
               filename ; for named files
               (let ((case-fold-search t)) ;; case-insensitive
                (string-match-p ,ext-regexp filename))) ; file name matches the regexp
         (+log! "Running %d `%s' hooks." (length ,hook-name) ',hook-name)
         (advice-remove 'find-file-noselect #',fn-name)
         (provide ',feature-name)
         (run-hooks ',hook-name)))
       (if (daemonp)
           ;; Load immediately after init when in daemon mode
           (add-hook 'after-init-hook (lambda () (provide ',feature-name) (run-hooks ',hook-name)) 90)
         (advice-add 'find-file-noselect :before #',fn-name '((depth . ,(if filetype -100 -101))))))))

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

(defun +setq-hook-fns (hooks rest &optional singles)
  (unless (or singles (= 0 (% (length rest) 2)))
    (signal 'wrong-number-of-arguments (list #'evenp (length rest))))
  (cl-loop with vars = (let ((args rest)
                             vars)
                         (while args
                           (push (if singles
                                     (list (pop args))
                                   (cons (pop args) (pop args)))
                                 vars))
                         (nreverse vars))
           for hook in (+resolve-hook-forms hooks)
           append
           (cl-loop for (var . val) in vars
                    collect
                    (list var val hook
                          (intern (format "+setq--%s-in-%s-h"
                                          var hook))))))

;; From Doom Emacs
(defmacro +add-hook! (hooks &rest rest)
  "A convenience macro for adding N functions to M hooks.

This macro accepts, in order:

  1. The mode(s) or hook(s) to add to. This is either an unquoted mode, an
     unquoted list of modes, a quoted hook variable or a quoted list of hook
     variables.
  2. Optional properties :local, :append, and/or :depth [N], which will make the
     hook buffer-local or append to the list of hooks (respectively),
  3. The function(s) to be added: this can be a quoted function, a quoted list
     thereof, a list of `defun' or `cl-defun' forms, or arbitrary forms (will
     implicitly be wrapped in a lambda).

If the hook function should receive an argument (like in
`enable-theme-functions'), the `args' variable can be expanded in the forms

  (+add-hook! \\='enable-theme-functions
    (message \"Enabled theme: %s\" (car args)))

\(fn HOOKS [:append :local [:depth N]] FUNCTIONS-OR-FORMS...)"
  (declare (indent (lambda (indent-point state)
                     (goto-char indent-point)
                     (when (looking-at-p "\\s-*(")
                       (lisp-indent-defform state indent-point))))
           (debug t))
  (let* ((hook-forms (+resolve-hook-forms hooks))
         (func-forms ())
         (defn-forms ())
         append-p local-p remove-p depth)
    (while (keywordp (car rest))
      (pcase (pop rest)
        (:append (setq append-p t))
        (:depth  (setq depth (pop rest)))
        (:local  (setq local-p t))
        (:remove (setq remove-p t))))
    (while rest
      (let* ((next (pop rest))
             (first (car-safe next)))
        (push (cond ((memq first '(function nil))
                     next)
                    ((eq first 'quote)
                     (let ((quoted (cadr next)))
                       (if (atom quoted)
                           next
                         (when (cdr quoted)
                           (setq rest (cons (list first (cdr quoted)) rest)))
                         (list first (car quoted)))))
                    ((memq first '(defun cl-defun))
                     (push next defn-forms)
                     (list 'function (cadr next)))
                    ((prog1 `(lambda (&rest args) ,@(cons next rest))
                       (setq rest nil))))
              func-forms)))
    `(progn
       ,@defn-forms
       (dolist (hook (nreverse ',hook-forms))
        (dolist (func (list ,@func-forms))
         ,(if remove-p
              `(remove-hook hook func ,local-p)
            `(add-hook hook func ,(or depth append-p) ,local-p)))))))

;; From Doom Emacs
(defmacro +remove-hook! (hooks &rest rest)
  "A convenience macro for removing N functions from M hooks.

Takes the same arguments as `add-hook!'.

If N = 1 and M = 1, there's no benefit to using this macro over `remove-hook'.

\(fn HOOKS [:append :local] FUNCTIONS)"
  (declare (indent defun) (debug t))
  `(+add-hook! ,hooks :remove ,@rest))

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
   (cl-loop for (var val hook fn) in (+setq-hook-fns hooks var-vals)
            collect `(defun ,fn (&rest args)
                      ,(format "%s = %s" var (pp-to-string val))
                      (setq-local ,var ,val))
            collect `(add-hook ',hook #',fn -90))))

;; From Doom Emacs
(defmacro +unsetq-hook! (hooks &rest vars)
  "Unbind setq hooks on HOOKS for VARS.

\(fn HOOKS &rest VAR1 VAR2...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (_var _val hook fn)
            in (+setq-hook-fns hooks vars 'singles)
            collect `(remove-hook ',hook #',fn))))

;; Adapted from: Doom Emacs
(defun +compile-functions (&rest fns)
  "Queue FNS to be byte/natively-compiled after a brief delay."
  (dolist (fn fns)
    (+eval-when-idle!
      (or (and (featurep 'native-compile)
               (or (subr-native-elisp-p (indirect-function fn))
                   ;; Do not log to `comp-log-buffer-name'
                   (cl-letf (((symbol-function 'comp-log-to-buffer) #'ignore))
                     (+shutup! (ignore-errors (native-compile fn))))))
          (byte-code-function-p fn)
          (let (byte-compile-warnings)
            (+shutup! (byte-compile fn)))))))

(defvar +shell-command-switch
  (pcase shell-file-name
    ((rx bol "fish" eol) "-lc")
    ((rx bol (or "tsch" "csh") eol) "-dc")
    (_ "-ilc")))

;; Inspired by: emacs.stackexchange.com/a/21432/37002
(defun +shell-command-to-string-ignore-stderr (command)
  "Execute shell command COMMAND and return its output as a string.

Works like `shell-command-to-string' with two differences:
1. It uses `+shell-command-switch' instead of `shell-command-switch'.
2. It returns only stdout and ignore the output of stderr."
  (with-output-to-string
    (with-current-buffer standard-output
      (let ((process-environment (append (list "TERM=smart") process-environment))) ; do not pretend to be dumb (aka, don't exit early when loading .bashrc)
        (process-file shell-file-name nil '(t nil) nil +shell-command-switch command)))))

(defun +env-save ()
  "Load environment variables from shell and save them to `+env-file'."
  (interactive)
  (unless os/win
    (with-temp-buffer
      (insert ";; -*- mode: emacs-lisp; no-byte-compile: t; no-native-compile: t; -*-\n\n")
      (let ((env-vars
             (mapcar ; Get environment variables from shell into an alist
              (lambda (line) (let ((var-val (string-split line "="))) (cons (car var-val) (string-join (cdr var-val) "="))))
              ;; "env --null" ends lines with null byte instead of newline
              (string-split (+shell-command-to-string-ignore-stderr "env --null") "\0"))))
        ;; Special treatment for the "PATH" variable, save it to `exec-path'
        (when-let ((path (alist-get "PATH" env-vars nil nil #'string=)))
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
              (insert (format "(setenv \"%s\" \"%s\")\n" (car env-var) value))))))
      (write-file +env-file))))

(defun +env-load ()
  "Load environment variables from `+env-file'."
  (interactive)
  (unless os/win
    (unless (file-exists-p +env-file) (+env-save))
    (+load +env-file)))

(defun +ignore-root (&rest roots)
  "Add ROOTS to ignored projects, recentf, etc."
  (dolist (root roots)
    (with-eval-after-load 'recentf
      (add-to-list 'recentf-exclude (rx-to-string `(or ,root ,(expand-file-name root)))))))

(defun +package-disabled-p (package &optional module)
  "Is package PACKAGE disabled in `minemacs-disabled-packages'.

Optionally, check also for the containing MODULE."
  (or
   (and (memq package (apply #'append (mapcar #'ensure-list minemacs-disabled-packages))) t)
   (and module (not (memq module (append minemacs-core-modules minemacs-modules))))))

(defvar +package--download-urls nil)

(defun +package-download-from-urls (pkgname &rest args)
  "Download PKGNAME files from URLs in ARGS.

Pass `:redownload' to force redownloading the package files.
Returns the load path of the package, useful for usage with `use-package''s
`:load-path'."
  (let* ((pkg-load-path (+directory-ensure minemacs-extra-packages-dir (format "%s/" pkgname)))
         (default-directory pkg-load-path)
         (redownload-p (memq :redownload args))
         (urls (remq :redownload args)))
    (add-to-list '+package--download-urls (cons pkgname urls))
    (dolist (url urls)
      (when-let* ((url-file-name (url-filename (url-generic-parse-url url)))
                  (url-file-name (file-name-nondirectory url-file-name))
                  (url-file-name (car (string-split url-file-name "?")))) ;; url/file.el?h=tag
        (when (and redownload-p (file-exists-p url-file-name))
          (delete-file url-file-name))
        (unless (file-exists-p url-file-name)
          (url-copy-file url url-file-name))))
    pkg-load-path))

(defun minemacs-run-build-functions (&optional dont-ask-p)
  "Run all build functions in `minemacs-build-functions'.

Call functions without asking when DONT-ASK-P is non-nil."
  (interactive "P")
  (dolist (fn minemacs-build-functions)
    (message "[MinEmacs]: Running `%s'" fn)
    (if dont-ask-p
        ;; Do not ask before installing
        (cl-letf (((symbol-function 'yes-or-no-p) #'always)
                  ((symbol-function 'y-or-n-p) #'always))
          (funcall-interactively fn))
      (funcall-interactively fn))))

(defun minemacs--bump-packages ()
  "Bump MinEmacs packages to the latest revisions."
  ;; Backup the current installed versions, this file can be restored if version
  ;; upgrade does break some packages.
  (message "[MinEmacs]: Creating backups for the current versions of packages")
  (let* ((backup-dir (concat minemacs-local-dir "minemacs/versions/"))
         (dest-file (concat backup-dir (format-time-string "default-%Y%m%d%H%M%S.el")))
         (src-file (concat straight-base-dir "straight/versions/default.el")))
    (unless (file-directory-p backup-dir) (mkdir backup-dir 'parents))
    (when (file-exists-p src-file)
      (message "[MinEmacs]: Creating backup from \"%s\" to \"%s\"" src-file dest-file)
      (copy-file src-file dest-file)))

  ;; Update straight recipe repositories
  (straight-pull-recipe-repositories)

  ;; Run `straight's update cycle, taking into account the explicitly pinned
  ;; packages versions.
  (message "[MinEmacs]: Pulling packages")
  (straight-x-pull-all)
  (message "[MinEmacs]: Freezing packages")
  (straight-x-freeze-versions)
  (message "[MinEmacs]: Rebuilding packages")
  (straight-rebuild-all)

  ;; Updating packages installed from URLs
  (message "[MinEmacs]: Updating packages installed from URLs")
  (mapc (lambda (args) (apply #'+package-download-from-urls (append args '(:redownload)))) +package--download-urls)

  ;; Run package-specific build functions (ex: `pdf-tools-install')
  (message "[MinEmacs]: Running additional package-specific build functions")
  (minemacs-run-build-functions 'dont-ask))

(defun minemacs-bump-packages ()
  "Update MinEmacs packages to the last revisions (can cause breakages)."
  (interactive)
  (let ((default-directory minemacs-root-dir)
        (compilation-buffer-name-function (lambda (_) "" "*minemacs-bump-packages*")))
    (compile "make bump")))

(defun minemacs-restore-locked-packages (restore-from-backup)
  "Restore lockfile packages list. Takes into account the pinned ones.
When called with \\[universal-argument] or with RESTORE-FROM-BACKUP, it will
restore the lockfile from backups, not Git."
  (interactive "P")
  (let* ((lockfile (concat straight-base-dir "straight/versions/default.el"))
         (default-directory (vc-git-root lockfile))
         (backup-dir (concat minemacs-local-dir "minemacs/versions/")))
    ;; Update straight recipe repositories
    (straight-pull-recipe-repositories)
    (if (not restore-from-backup)
        (progn
          (message "[MinEmacs] Reverting file \"%s\" to the original" lockfile)
          (unless (zerop (vc-git-revert lockfile))
            ;; Signal an error when the `vc-git-revert' returns non-zero
            (user-error "[MinEmacs] An error occurred when trying to revert \"%s\"" lockfile)))
      (message "[MinEmacs] Trying to restore the lockfile from backups.")
      (if-let* ((_ (file-exists-p backup-dir))
                (backups (directory-files backup-dir nil "[^.][^.]?\\'"))
                (restore-backup-file (completing-read "Select which backup to restore: " backups))
                (last-backup (expand-file-name restore-backup-file backup-dir)))
          (if (not (file-exists-p last-backup))
              (user-error "[MinEmacs] No backup file")
            (copy-file last-backup lockfile 'overwrite-existing)
            (message "[MinEmacs] Restored the last backup from \"%s\"" restore-backup-file))))
    ;; This will ensure that the pinned lockfile is up-to-date
    (straight-x-freeze-pinned-versions)
    ;; Restore packages to the versions pinned in the lockfiles
    (when (file-exists-p (concat straight-base-dir "versions/pinned.el"))
      (message "[MinEmacs] Restoring pinned versions of packages")
      (straight-x-thaw-pinned-versions))
    (message "[MinEmacs] Restoring packages from the global lockfile versions")
    (straight-thaw-versions)
    ;; Rebuild the packages
    (message "[MinEmacs] Rebuilding packages")
    (straight-rebuild-all)
    ;; Run package-specific build functions (ex: `pdf-tools-install')
    (message "[MinEmacs] Running additional package-specific build functions")
    (minemacs-run-build-functions 'dont-ask)))

(defun minemacs-upgrade (pull-minemacs)
  "Upgrade MinEmacs and its packages to the latest pinned versions (recommended).

When PULL-MINEMACS is non-nil, run a \"git pull\" in MinEmacs' directory.

This calls `minemacs-update-restore-locked' asynchronously."
  (interactive "P")
  (let ((default-directory minemacs-root-dir)
        (compilation-buffer-name-function (lambda (_) "" "*minemacs-upgrade*"))
        (cmd (format "sh -c '%smake locked'" (if pull-minemacs "git pull && " ""))))
    (compile cmd)))

(defun +minemacs-root-dir-cleanup ()
  "Cleanup MinEmacs' root directory."
  (let ((default-directory minemacs-root-dir))
    (mapc (+apply-partially-right #'+delete-file-or-directory 'trash 'recursive)
          (directory-files minemacs-root-dir nil (rx (seq bol (or "eln-cache" "auto-save-list" "elpa") eol))))))



;;; Files, directories and IO helper functions

(defun +file-mime-type (file)
  "Get MIME type for FILE based on magic codes provided by the \"file\" command.
Return a symbol of the MIME type, ex: `text/x-lisp', `text/plain',
`application/x-object', `application/octet-stream', etc."
  (if-let ((file-cmd (executable-find "file"))
           (mime-type (shell-command-to-string (format "%s --brief --mime-type %s" file-cmd file))))
      (intern (string-trim-right mime-type))
    (error "The \"file\" command isn't installed")))

(defun +file-name-incremental (filename)
  "Return a unique file name for FILENAME.
If \"file.ext\" exists, returns \"file-0.ext\"."
  (let* ((ext (file-name-extension filename))
         (dir (file-name-directory filename))
         (file (file-name-base filename))
         (filename-regex (concat "^" file "\\(?:-\\(?1:[[:digit:]]+\\)\\)?" (if ext (concat "\\." ext) "")))
         (last-file (car (last (directory-files dir nil filename-regex))))
         (last-file-num (and last-file (string-match filename-regex last-file) (match-string 1 last-file)))
         (num (1+ (string-to-number (or last-file-num "-1")))))
    (file-name-concat dir (format "%s%s%s" file (if last-file (format "-%d" num) "") (if ext (concat "." ext) "")))))

(defun +file-read-to-string (filename)
  "Return a string with the contents of FILENAME."
  (when (and (file-exists-p filename) (not (file-directory-p filename)))
    (with-temp-buffer
      (insert-file-contents filename)
      (buffer-string))))

(defun +directory-subdirs (dir)
  "Return a list of sub-directories in DIR."
  (when dir
    (seq-filter #'file-directory-p
                (mapcar #'abbreviate-file-name (directory-files dir t "[^.][^.]?\\'")))))

(defun +directory-ensure (&rest path-parts)
  "Concatenate PATH-PARTS to construct a path and return it.

Ensure the path exists, if not create it. The exact behavior is to create the
parent directory if the path is a file, and if the path is a directory, create
that directory."
  (let* ((path (mapconcat #'identity path-parts nil))
         (parent-dir (file-name-directory path)))
    (unless (file-directory-p parent-dir)
      (ignore-errors (mkdir parent-dir t))
      (unless (file-directory-p parent-dir)
        (+error! "Cannot create directory %s" parent-dir)))
    path))

(defun +directory-root-containing-file (files &optional start-path)
  "Return the path containing a file from FILES starting from START-PATH."
  (let ((dir (or start-path (and buffer-file-name (file-name-directory buffer-file-name)) default-directory)))
    (catch 'root
      (while dir
        (when (cl-some #'file-exists-p (mapcar (+apply-partially-right #'expand-file-name dir) (ensure-list files)))
          (throw 'root dir))
        (setq dir (file-name-parent-directory dir))))))

(defun +delete-this-file (&optional path force-p)
  "Delete PATH.

If PATH is not specified, default to the current buffer's file.

If FORCE-P, delete without confirmation."
  (interactive
   (list (buffer-file-name (buffer-base-buffer))
         current-prefix-arg))
  (let* ((path (or path (buffer-file-name (buffer-base-buffer))))
         (short-path (abbreviate-file-name path)))
    (unless (and path (file-exists-p path))
      (user-error "Buffer is not visiting any file"))
    (unless (file-exists-p path)
      (error "File doesn't exist: %s" path))
    (unless (or force-p (y-or-n-p (format "Really delete %S?" short-path)))
      (user-error "Aborted"))
    (unwind-protect
        (progn (delete-file path delete-by-moving-to-trash) t)
      (when (file-exists-p path)
        (error "Failed to delete %S" short-path)))))

;; Rewrite of: crux-delete-file-and-buffer, proposes also to delete VC
;; controlled files even when `vc-delete-file' fails (edited, conflict, ...).
(defun +delete-this-file-and-buffer (&optional filename)
  "Delete FILENAME and its associated visiting buffer."
  (interactive)
  (when-let ((filename (or filename (buffer-file-name)))
             (short-path (abbreviate-file-name filename)))
    (if (vc-backend filename)
        (or (ignore-errors (vc-delete-file (buffer-file-name)))
            (+delete-this-file filename)
            (kill-buffer))
      (when (y-or-n-p (format "Are you sure you want to delete %s? " short-path))
        (delete-file filename delete-by-moving-to-trash)
        (message "Deleted file %s" short-path)
        (kill-buffer)))))

(defun +delete-file-or-directory (file-or-directory &optional trash recursive)
  "Delete FILE-OR-DIRECTORY with `delete-file' or `delete-directory'.

Move to trash when TRASH is non-nil, delete directories recursively when
RECURSIVE is non-nil."
  (if (file-directory-p file-or-directory)
      (delete-directory file-or-directory recursive trash)
    (delete-file file-or-directory trash)))

(if (fboundp 'rename-visited-file)
    (defalias '+move-this-file #'rename-visited-file)
  (defun +move-this-file (new-path &optional force-p)
    "Move current buffer's file to NEW-PATH.

If FORCE-P, overwrite the destination file if it exists, without confirmation."
    (interactive
     (list (read-file-name "Move file to: ")
           current-prefix-arg))
    (unless (and buffer-file-name (file-exists-p buffer-file-name))
      (user-error "Buffer is not visiting any file"))
    (let ((old-path (buffer-file-name (buffer-base-buffer)))
          (new-path (expand-file-name new-path)))
      (when (directory-name-p new-path)
        (setq new-path (expand-file-name (file-name-nondirectory old-path) new-path)))
      (make-directory (file-name-directory new-path) t)
      (rename-file old-path new-path (or force-p 1))
      (set-visited-file-name new-path t t)
      (message "File moved to %S" (abbreviate-file-name new-path)))))

(defun +tramp-sudo-file-path (file)
  "Construct a Tramp sudo path to FILE. Works for both local and remote files."
  (tramp-make-tramp-file-name "sudo" tramp-root-id-string nil (or (file-remote-p file 'host) "localhost") nil file))

(defun +sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (find-file (+tramp-sudo-file-path file)))

(defun +sudo-this-file ()
  "Open the current file as root."
  (interactive)
  (if-let ((this-file (or buffer-file-name
                          (when (derived-mode-p 'dired-mode 'wdired-mode)
                            default-directory))))
      (find-file (+tramp-sudo-file-path this-file))
    (user-error "Current buffer not bound to a file")))

(defun +sudo-save-buffer ()
  "Save this buffer as root. Save as new file name if called with prefix."
  (interactive)
  (if-let ((file (or (and (or (not buffer-file-name) current-prefix-arg)
                          (read-file-name "Save as root to: "))
                     buffer-file-name))
           (file (+tramp-sudo-file-path (expand-file-name file)))
           (dest-buffer (find-file-noselect file))
           (src-buffer (current-buffer)))
      (progn
        (copy-to-buffer dest-buffer (point-min) (point-max))
        (unwind-protect (with-current-buffer dest-buffer (save-buffer))
          (unless (eq src-buffer dest-buffer) (kill-buffer dest-buffer))
          (with-current-buffer src-buffer (revert-buffer t t))))
    (user-error "Unable to open %S" (abbreviate-file-name file))))

(defun +yank-this-file-name ()
  "Yank the file name of this buffer."
  (interactive)
  (if-let ((file (buffer-file-name)))
      (with-temp-buffer
        (insert file)
        (kill-ring-save (point-min) (point-max)))
    (user-error "This buffer isn't bound to a file")))

(defun +clean-file-name (filename &optional downcase-p)
  "Clean FILENAME, optionally convert to DOWNCASE-P."
  ;; Clean slashes, backslashes, ":", ";", spaces, and tabs
  (replace-regexp-in-string
   "[:;\t\n\r /\\_]+" "-"
   (replace-regexp-in-string
    "[‘’‚“”„\"`'()&]+" ""
    (if downcase-p (downcase filename) filename))))



;;; Exporter and converters

(defcustom +html2pdf-default-backend 'wkhtmltopdf
  "The default backend to convert HTML files to PDFs in `+html2pdf'."
  :group 'minemacs-utils
  :type '(choice
          (const wkhtmltopdf)
          (const htmldoc)
          (const weasyprint)
          (const pandoc+context)
          (const pandoc)))

(defcustom +html2pdf-backend-config-file nil
  "A config file to use with the backend tool (pandoc, weasyprint, ...)."
  :group 'minemacs-utils
  :type 'file)

(defun +html2pdf (infile outfile &optional backend)
  "Convert HTML file INFILE to PDF and save it to OUTFILE.
When BACKEND is provided, the corresponding program is used, otherwise, the
value of `+html2pdf-default-backend' is used."
  (if-let ((default-directory (file-name-directory infile))
           (backend (or backend +html2pdf-default-backend))
           (backend-command
            (pcase backend
              ('wkhtmltopdf
               (list "wkhtmltopdf"
                     "--images" "--disable-javascript" "--enable-local-file-access"
                     "--encoding" "utf-8"
                     infile outfile))
              ('htmldoc
               (list "htmldoc"
                     "--charset" "utf-8"
                     "--bodyfont" "sans" "--textfont" "sans" "--headfootfont" "sans"
                     "--top" "50#mm" "--bottom" "50#mm" "--right" "50#mm" "--left" "50#mm"
                     "--fontsize" "10"
                     "--size" "a4"
                     "--continuous"
                     "--outfile" outfile infile))
              ('weasyprint
               (list "weasyprint"
                     "--encoding" "utf-8"
                     "--stylesheet" (or +html2pdf-backend-config-file
                                        (expand-file-name "templates/+html2pdf/weasyprint-pdf.css" minemacs-assets-dir))
                     infile outfile))
              ('pandoc+context
               (list "pandoc"
                     "--pdf-engine=context"
                     "--variable" "fontsize=10pt"
                     "--variable" "linkstyle=slanted"
                     "-o" outfile infile))
              ('pandoc
               (list "pandoc"
                     "--defaults" (or +html2pdf-backend-config-file
                                      (expand-file-name "templates/+html2pdf/pandoc.yaml" minemacs-assets-dir))
                     "-o" outfile infile)))))
      (apply #'call-process (append (list (car backend-command) nil nil nil) (cdr backend-command)))
    (user-error "Backend \"%s\" not available" backend)))

(defun +txt2html (infile outfile &optional mail-mode-p)
  "Convert plain-text file INFILE to HTML and save it to OUTFILE.
When MAIL-MODE-P is non-nil, --mailmode is passed to \"txt2html\"."
  (apply #'call-process (append '("txt2html" nil nil nil "-8")
                                (when mail-mode-p '("--mailmode"))
                                (list "--outfile" outfile infile))))

(defvar +save-as-pdf-filename nil
  "File name to use, if non-nil, for the output file.")

(defun +save-as-pdf (infile &optional mail-mode-p)
  "Save URL as PDF.
This function's signature is compatible with `browse-url-browser-function'
so it can be used to save HTML pages or emails to PDF.
When MAIL-MODE-P is non-nil, treat INFILE as a mail."
  (let* ((infile (string-trim-left infile "file://"))
         (outfile (+file-name-incremental
                   (or +save-as-pdf-filename
                       (expand-file-name
                        (file-name-with-extension (file-name-base infile) ".pdf")
                        (file-name-directory infile))))))
    (if (zerop
         ;; For HTML files (with extension ".html" or ".htm"), just call `+html2pdf'
         (if (string-match-p "^html?$" (file-name-extension infile))
             (+html2pdf infile outfile)
           ;; For non-HTML (plain-text) files, convert them to HTML then call `+html2pdf'
           (let ((tmp-html (make-temp-file "txt2html-" nil ".html")))
             (+txt2html infile tmp-html mail-mode-p)
             (+html2pdf tmp-html outfile))))
        (message "Exported PDF to %S"
                 (truncate-string-to-width (abbreviate-file-name outfile) (/ (window-width (minibuffer-window)) 2) nil nil t))
      (user-error (if (file-exists-p outfile) "PDF created but with some errors!" "An error occurred, cannot create the PDF!")))))

(defcustom +single-file-executable "single-file"
  "The executable for \"single-file\" which is used archive HTML pages."
  :type 'string
  :group 'minemacs)

(defun +single-file (url out-file)
  "Save URL into OUT-FILE as a standalone HTML file."
  (interactive
   (let ((url (or (thing-at-point 'url) (read-string "URL to save: "))))
     (list url (read-file-name "Save to: " nil nil nil (url-filename (url-generic-parse-url url))))))
  (if (executable-find +single-file-executable)
      (make-process
       :name "single-file-cli"
       :buffer "*single-file*"
       :command (list +single-file-executable
                      "--browser-executable-path" browse-url-chromium-program
                      url (expand-file-name out-file)))
    (user-error "Please set `+single-file-executable' accordingly")))



;;; Serial port

(autoload 'term-send-string "term")
(defcustom +serial-port "/dev/ttyUSB0"
  "The default port (device) to use."
  :group 'minemacs-serial
  :type 'file)

(defcustom +serial-baudrate 115200
  "The default baudrate to use."
  :group 'minemacs-serial
  :type 'natnum)

(defcustom +serial-first-commands nil
  "A list of commands to run in the serial terminal after creation."
  :group 'minemacs-serial
  :type '(repeat string))

(defvar +serial-buffer nil)
(defvar +serial-process nil)

(defun +serial-running-p ()
  "Is there a serial port terminal running?"
  (buffer-live-p +serial-buffer) (process-live-p +serial-process))

(defun +serial--run-commands (port baud &rest commands)
  "Run COMMANDS on a device via serial communication.

Connect at PORT with baudrate BAUD."
  (let ((commands (ensure-list commands)))
    (unless (+serial-running-p)
      (setq +serial-buffer (serial-term port baud)
            +serial-process (get-buffer-process +serial-buffer)
            commands (append +serial-first-commands commands)))
    (if (+serial-running-p)
        (term-send-string +serial-process (string-join (append commands '("\n")) "\n"))
      (user-error "Unable to communicate with the serial terminal process"))))

(defun +serial-run-commands (commands &optional port baud)
  "Run COMMANDS on a device via serial communication.

If PORT or BAUD are nil, use values from `+serial-port' and `+serial-baudrate'."
  (interactive (list (read-shell-command (format "Run command via serial port: "))))
  (let ((port (or port +serial-port))
        (baud (or baud +serial-baudrate)))
    (+log! "Dev %s@%d: running commands %S" port baud commands)
    (apply #'+serial--run-commands (append (list port baud) (ensure-list commands)))))



;;; Networking

(defvar +net-default-device "wlan0")

(defun +net-get-ip-address (&optional dev)
  "Get the IP-address for device DEV (default: eth0) of the current machine."
  (let ((dev (or dev +net-default-device)))
    (format-network-address (car (network-interface-info dev)) t)))



;;; Github

(defun +github-latest-release (repo &optional fallback-release)
  "Get the latest release of REPO. Strips the \"v\" at left.

Fallback to FALLBACK-RELEASE when it can't get the last one."
  (if-let ((latest
            (ignore-errors
              (with-temp-buffer
                (url-insert-file-contents
                 (format "https://api.github.com/repos/%s/releases/latest" repo))
                (json-parse-buffer :object-type 'plist)))))
      (string-trim-left
       (car (last (split-string (plist-get latest :html_url) "/")))
       "v")
    fallback-release))



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
  (when-let ((pid (+lock--locker-pid name)))
    (and (process-attributes pid) t)))

(defun +locked-by-this-process-p (name)
  "Return non-nil if the resource NAME locked by this Emacs instance."
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
  (when (or force-p (+locked-by-this-process-p name))
    (+info! "Resource `%s' unlocked" name)
    (delete-file (+lock--file name))
    t))



;;; Directory local tweaks & hacks

(defun +dir-locals-reload-for-this-buffer ()
  "Reload directory-local for the current buffer."
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)
    (+info! "Reloaded directory-local variables for buffer %s"
            (buffer-name (current-buffer)))))

(defun +dir-locals-reload-for-all-buffers-in-this-directory ()
  "Reload dir-locals for all buffers in the current `default-directory'."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir)
          (+dir-locals-reload-for-this-buffer))))))

(defun +dir-locals--autoreload-h ()
  "Is it relevant to auto reload dir-locals for his buffer."
  (when (and (buffer-file-name)
             (equal dir-locals-file (file-name-nondirectory (buffer-file-name))))
    (+dir-locals-reload-for-all-buffers-in-this-directory)
    (message "Reloaded directory-local variables defined in %s." dir-locals-file)))

(defvar +dir-locals--autoreload-p nil)

(defun +dir-locals-toggle-autoreload (&optional enable)
  "Toggle autoloading dir-local variables after editing the \".dir-locals\" file.

If ENABLE is non-nil, force enabling autoreloading."
  (interactive)
  (if (or enable (not +dir-locals--autoreload-p))
      (progn
        (remove-hook 'after-save-hook #'+dir-locals--autoreload-h)
        (setq +dir-locals--autoreload-p nil)
        (message "Disabled auto-reloading directory-locals."))
    (add-hook 'after-save-hook #'+dir-locals--autoreload-h)
    (setq +dir-locals--autoreload-p t)
    (message "Enabled auto-reloading directory-locals.")))

(defun +dir-locals-open-or-create ()
  "Open or create the dir-locals.el for the current project."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (base-dir (car (ensure-list (dir-locals-find-file file-name)))))
    (find-file
     (cond (base-dir (expand-file-name dir-locals-file base-dir))
           ((project-current) (expand-file-name dir-locals-file (project-root (project-current))))
           ((vc-root-dir) (expand-file-name dir-locals-file (vc-root-dir)))
           (t (expand-file-name dir-locals-file (file-name-directory file-name)))))))



;;; Misc emacs tweaks

;; Adapted from: rougier/nano-emacs
(defun +what-faces (pos)
  "Get the font faces at POS."
  (interactive "d")
  (let ((faces (remq nil
                     (list
                      (get-char-property pos 'read-face-name)
                      (get-char-property pos 'face)
                      (plist-get (text-properties-at pos) 'face)))))
    (message "Faces: %s" faces)))

(defcustom +screenshot-delay 5
  "A delay to wait before taking the screenshot.
Applicable only when calling `+screenshot-svg' with a prefix."
  :group 'minemacs-utils
  :type 'number)

;; Inspired by: reddit.com/r/emacs/comments/idz35e/comment/g2c2c6y
(defun +screenshot-svg (outfile)
  "Save a screenshot of the current frame as an SVG image to OUTFILE.

If launched with a prefix or universal argument, it waits for a moment (defined
by `+screenshot-delay') before taking the screenshot."
  (interactive "FSave to file: ")
  (let ((outfile (file-name-with-extension outfile "svg")))
    (if current-prefix-arg
        (run-with-timer +screenshot-delay nil (apply-partially #'+screenshot-svg--take-screenshot outfile))
      (+screenshot-svg--take-screenshot outfile))))

(defun +screenshot-svg--take-screenshot (&optional outfile)
  "Save a SVG screenshot of the current frame to OUTFILE."
  (let* ((tmp-file (make-temp-file "emacs-" nil ".svg"))
         (data (x-export-frames nil 'svg)))
    (with-temp-file tmp-file (insert data))
    (when (stringp outfile) (copy-file tmp-file outfile))
    (message "Screenshot saved to %s" (or outfile tmp-file))))

;; Kill the minibuffer even when in another windown.
;; Adapted from: trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html
(defun +minibuffer-kill-minibuffer ()
  "Kill the minibuffer when switching to window with mouse."
  (interactive)
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(defun +region-or-thing-at-point ()
  "Return the region or the thing at point."
  (when-let ((thing (ignore-errors
                      (or (prog1 (thing-at-point 'region t)
                            (deactivate-mark))
                          (cl-some (+apply-partially-right #'thing-at-point t)
                                   '(symbol email number string word))))))
    ;; If the matching thing has multi-lines, join them
    (string-join (string-lines thing))))

(defvar +webjump-read-string-initial-query nil)

(defun +webjump-read-string-with-initial-query (prompt)
  (let ((input (read-string (concat prompt ": ") +webjump-read-string-initial-query)))
    (unless (webjump-null-or-blank-string-p input) input)))

(defun +webjump ()
  "Like `webjump', with initial query filled from `+region-org-thing-at-point'."
  (interactive)
  (require 'webjump)
  (let ((+webjump-read-string-initial-query (+region-or-thing-at-point)))
    (cl-letf (((symbol-function 'webjump-read-string) #'+webjump-read-string-with-initial-query))
      (webjump))))

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
              (when-let ((tab-num (seq-position (tabspaces--list-tabspaces) ,tab-name #'string=)))
               (tabspaces-close-workspace (1+ tab-num)))
            ;; Or default to the built-in `tab-bar'.
            (when-let ((tab-num (seq-position (tab-bar-tabs) ,tab-name (lambda (tab name) (string= name (alist-get 'name tab))))))
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
  '(c++-mode c++-ts-mode c-mode c-ts-mode python-mode python-ts-mode rust-mode
    rust-ts-mode cmake-mode js-mode js-ts-mode typescript-mode
    typescript-ts-mode json-mode json-ts-mode js-json-mode)
  "Modes for which Eglot can be automatically enabled by `+eglot-auto-enable'."
  :group 'minemacs-prog
  :type '(repeat symbol))

(defun +eglot--ensure-maybe-h ()
  "Maybe auto start Eglot if the current mode is in `+eglot-auto-enable-modes'."
  (when (memq major-mode +eglot-auto-enable-modes)
    (eglot-ensure)))

(defun +eglot-auto-enable ()
  "Auto-enable Eglot in configured modes in `+eglot-auto-enable-modes'."
  (interactive)
  (add-hook 'after-change-major-mode-hook #'+eglot--ensure-maybe-h)
  (remove-hook 'after-change-major-mode-hook #'+lsp--ensure-maybe-h))

(defun +eglot-use-on-all-supported-modes (mode-list)
  "Add all modes in MODE-LIST to `+eglot-auto-enable-modes'."
  (dolist (mode-def mode-list)
    (let ((mode (if (listp mode-def) (car mode-def) mode-def)))
      (cond
       ((listp mode) (+eglot-use-on-all-supported-modes mode))
       (t
        (when (and (not (eq 'clojure-mode mode)) ; prefer cider
                   (not (eq 'lisp-mode mode))    ; prefer sly
                   (not (eq 'scheme-mode mode))) ; prefer geiser
          (add-to-list '+eglot-auto-enable-modes mode)))))))

(defun +eglot-register (modes &rest servers)
  "Register MODES with LSP SERVERS.
Examples:
  (+eglot-register 'vhdl-mode \"vhdl_ls\")
  (+eglot-register 'lua-mode \"lua-language-server\" \"lua-lsp\")
  (+eglot-register '(c-mode c++-mode) '(\"clangd\" \"--clang-tidy\" \"-j=12\") \"ccls\")"
  (declare (indent 0))
  (with-eval-after-load 'eglot
    (add-to-list
     'eglot-server-programs
     (cons modes (if (length> servers 1)
                     (eglot-alternatives (ensure-list servers))
                   (ensure-list (car servers)))))))

;; From: github.com/MaskRay/ccls/wiki/eglot#misc
(defun +eglot-ccls-inheritance-hierarchy (&optional derived)
  "Show inheritance hierarchy for the thing at point.
If DERIVED is non-nil (interactively, with prefix argument), show
the children of class at point."
  (interactive "P")
  (if-let* ((res (jsonrpc-request
                  (eglot--current-server-or-lose)
                  :$ccls/inheritance
                  (append (eglot--TextDocumentPositionParams)
                          `(:derived ,(if derived t :json-false))
                          '(:levels 100) '(:hierarchy t))))
            (tree (list (cons 0 res))))
      (with-help-window "*ccls inheritance*"
        (with-current-buffer standard-output
          (while tree
            (pcase-let ((`(,depth . ,node) (pop tree)))
              (cl-destructuring-bind (&key uri range) (plist-get node :location)
                (insert (make-string depth ?\ ) (plist-get node :name) "\n")
                (make-text-button
                 (+ (pos-bol 0) depth) (pos-eol 0)
                 'action (lambda (_arg)
                           (interactive)
                           (find-file (eglot--uri-to-path uri))
                           (goto-char (car (eglot--range-region range)))))
                (cl-loop for child across (plist-get node :children)
                         do (push (cons (1+ depth) child) tree)))))))
    (eglot--error "Hierarchy unavailable")))



;;; Binary files tweaks

(defgroup minemacs-binary nil
  "MinEmacs binary files."
  :group 'minemacs)

(defcustom +binary-objdump-executable "objdump"
  "The \"objdump\" command."
  :group 'minemacs-binary
  :type '(choice file string))

(defcustom +binary-objdump-enable t
  "Enable or disable disassembling suitable files with objdump."
  :group 'minemacs-binary
  :type 'boolean)

(defcustom +binary-hexl-enable t
  "Enable or disable opening suitable files in `hexl-mode'."
  :group 'minemacs-binary
  :type 'boolean)

(defun +binary-objdump-p (filename)
  "Can FILENAME be recognized by \"objdump\"."
  (when-let* ((file (and filename (file-truename filename))))
    (and +binary-objdump-executable
         (executable-find +binary-objdump-executable)
         (not (file-remote-p file))
         (file-exists-p file)
         (not (file-directory-p file))
         (not (zerop (file-attribute-size (file-attributes file))))
         (not (string-match-p "file format not recognized"
                              (shell-command-to-string
                               (format "%s --file-headers %s"
                                       +binary-objdump-executable
                                       (shell-quote-argument file))))))))

(defun +binary-objdump-buffer-p (&optional buffer)
  "Can the BUFFER be viewed as a disassembled code with objdump."
  (and +binary-objdump-enable (+binary-objdump-p (buffer-file-name buffer))))

;; A predicate for detecting binary files. Inspired by:
;; emacs.stackexchange.com/q/10277/37002)
(defun +binary-buffer-p (&optional buffer)
  "Return whether BUFFER or the current buffer is binary.

A binary buffer is defined as containing at least one null byte.

Returns either nil, or the position of the first null byte."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (search-forward (string ?\x00) nil t 1))))

(defun +binary-file-p (file &optional chunk)
  "Is FILE a binary?

This checks the first CHUNK of bytes, defaults to 1024."
  (with-temp-buffer
    (insert-file-contents-literally file nil 0 (or chunk 1024))
    (goto-char (point-min))
    (search-forward (string ?\x00) nil t 1)))

(defun +binary-hexl-buffer-p (&optional buffer)
  "Does BUFFER (defaults to the current buffer) should be viewed using `hexl-mode'."
  (and +binary-hexl-enable
       (+binary-buffer-p buffer)
       ;; Executables are viewed with objdump mode
       (not (+binary-objdump-buffer-p buffer))))

(define-derived-mode objdump-disassemble-mode
  special-mode "Objdump Mode"
  "Major mode for viewing executable files disassembled using objdump."
  (if-let* ((file (buffer-file-name))
            (objdump-file (+binary-objdump-p file)))
      (let ((buffer-read-only nil))
        (message "Disassembling %S using objdump." (file-name-nondirectory file))
        (erase-buffer)
        (set-visited-file-name (file-name-with-extension file "_dias.objdump"))
        (call-process "objdump" nil (current-buffer) nil "-d" file)
        (goto-char (point-min))
        (buffer-disable-undo)
        (set-buffer-modified-p nil)
        (view-mode 1)
        (read-only-mode 1)
        ;; Apply syntax highlighting from `asm-mode'
        (require 'asm-mode)
        (set-syntax-table (make-syntax-table asm-mode-syntax-table))
        (modify-syntax-entry ?# "< b") ; use # for comments
        (setq-local font-lock-defaults '(asm-font-lock-keywords)))
    (user-error "Objdump can not be used with this buffer")))

(defun +binary-hexl-mode-maybe ()
  "Activate `hexl-mode' if relevant for the current buffer."
  (interactive)
  (when (and (not (eq major-mode 'hexl-mode)) (+binary-hexl-buffer-p))
    (hexl-mode 1)))

(defun +binary-setup-modes ()
  "Setup binary modes (objdump and hexl) for relevant buffers and file types."
  (add-to-list 'magic-fallback-mode-alist '(+binary-objdump-buffer-p . objdump-disassemble-mode) t)
  (add-to-list 'magic-fallback-mode-alist '(+binary-hexl-buffer-p . +binary-hexl-mode-maybe) t))



;;; Buffer related tweaks

(defgroup minemacs-buffer nil
  "MinEmacs buffer stuff."
  :group 'minemacs)

;; From: emacswiki.org/emacs/download/misc-cmds.el
;; Candidate as a replacement for `kill-buffer', at least when used interactively.
;; For example: (define-key global-map [remap kill-buffer] 'kill-buffer-and-its-windows)
;; We cannot just redefine `kill-buffer', because some programs count on a
;; specific other buffer taking the place of the killed buffer (in the window).
(defun +kill-buffer-and-its-windows (buffer &optional msgp)
  "Kill BUFFER and delete its windows.  Default is `current-buffer'.
BUFFER may be either a buffer or its name (a string)."
  (interactive (list (read-buffer "Kill buffer: " (current-buffer) 'existing) 'MSGP))
  (setq buffer (get-buffer buffer))
  (if (buffer-live-p buffer) ; Kill live buffer only.
      (let ((wins (get-buffer-window-list buffer nil t))) ; On all frames.
        (when (kill-buffer buffer) ; Only delete windows if buffer killed.
          (dolist (win wins) ; (User might keep buffer if modified.)
            (when (window-live-p win)
              ;; Ignore error, in particular,
              ;; "Attempt to delete the sole visible or iconified frame".
              (condition-case nil (delete-window win) (error nil))))))
    (when msgp (user-error "Cannot kill buffer.  Not a live buffer: `%s'" buffer))))

;; From: emacswiki.org/emacs/download/misc-cmds.el
(defun +region-to-buffer (start end buffer arg)
  "Copy region to BUFFER: At beginning (prefix >= 0), end (< 0), or replace.
START and END are the region boundaries.
BUFFER is a buffer or its name (a string).
With prefix ARG >= 0: `append-to-buffer':
  Append contents of region to end of BUFFER.
  (Point is moved to end of BUFFER first.)
With prefix ARG < 0:  `prepend-to-buffer':
  Prepend contents of region to beginning of BUFFER.
  (Point is moved to beginning of BUFFER first.)
With no prefix ARG (nil): `copy-to-buffer'.
  Write region to BUFFER, replacing any previous contents."
  (interactive
   (let ((arg (and current-prefix-arg (prefix-numeric-value current-prefix-arg))))
     (list (region-beginning)
           (region-end)
           (read-buffer
            (concat (if arg
                        (if (natnump arg) "Append" "Prepend")
                      "Write")
                    " region to buffer: ")
            (if (fboundp 'another-buffer) ; Defined in `misc-fns.el'.
                (another-buffer nil t)
              (other-buffer (current-buffer))))
           arg)))
  (setq buffer (get-buffer-create buffer)) ; Convert to buffer.
  (when (eq buffer (current-buffer)) (error "Cannot copy region to its own buffer"))
  (cond ((natnump arg)
         (with-current-buffer buffer (goto-char (point-max)))
         (append-to-buffer buffer start end))
        (arg
         (with-current-buffer buffer (goto-char (point-min)))
         (prepend-to-buffer buffer start end))
        (t (copy-to-buffer buffer start end))))

;; From: emacswiki.org/emacs/download/misc-cmds.el
(defun +region-to-file (start end filename arg)
  "With prefix arg, this is `append-to-file'.  Without, it is `write-region'.
START and END are the region boundaries.
Prefix ARG non-nil means append region to end of file FILENAME.
Prefix ARG nil means write region to FILENAME, replacing contents."
  (interactive
   (list (region-beginning)
         (region-end)
         (read-file-name (concat (if current-prefix-arg "Append" "Write")
                                 " region to file: "))
         current-prefix-arg))
  (let* ((curr-file (buffer-file-name))
         (same-file-p (and curr-file (string= curr-file filename))))
    (cond ((or (not same-file-p)
               (progn (when (fboundp 'flash-ding) (flash-ding))
                      (yes-or-no-p
                       (format
                        "Do you really want to REPLACE the contents of `%s' by just the REGION? "
                        (file-name-nondirectory curr-file)))))
           (write-region start end filename arg)
           (when same-file-p (revert-buffer t t)))
          (t (message "OK.  Not written.")))))

(defun +kill-some-buffers (&optional list)
  "Kill some buffers.  Asks the user whether to kill the modified ones.
Non-interactively, if optional argument LIST is non-nil, it
specifies the list of buffers to kill, asking for approval for each one.
See `kill-some-buffers'."
  (interactive)
  ;; Replace the `kill-buffer-ask' locally (used by `kill-some-buffers')
  ;; with our function which don't ask about unmodified buffers.
  (cl-letf (((symbol-function 'kill-buffer-ask) #'+kill-buffer-ask-if-modified))
    (kill-some-buffers list)))

(defcustom +kill-buffer-no-ask-list
  (list (or (bound-and-true-p messages-buffer-name) "*Messages*") "*Warnings*")
  "A list of buffer names to be killed without confirmation."
  :group 'minemacs-buffer
  :type '(repeat string))

(with-eval-after-load 'comp
  (when (featurep 'native-compile)
    (setq
     +kill-buffer-no-ask-list
     (append +kill-buffer-no-ask-list
             (ensure-list (bound-and-true-p comp-async-buffer-name))
             (ensure-list (bound-and-true-p comp-log-buffer-name))))))

(defun +kill-buffer-ask-if-modified (buffer)
  "Like `kill-buffer-ask', but kills BUFFER without confirmation when unmodified.
Kill without asking for buffer names in `+kill-buffer-no-ask-list'."
  (when (or (not (buffer-modified-p buffer))
            (member (buffer-name buffer) +kill-buffer-no-ask-list)
            (yes-or-no-p (format "Buffer %s HAS BEEN MODIFIED.  Kill? "
                                 (buffer-name buffer))))
    (kill-buffer buffer)))

;; From: emacswiki.org/emacs/download/misc-cmds.el
(defun +delete-extra-windows-for-buffer ()
  "Delete all other windows showing the selected window's buffer."
  (interactive)
  (let* ((selwin (selected-window))
         (buf (window-buffer selwin)))
    (walk-windows
     (lambda (ww)
       (unless (eq ww selwin)
         (when (eq (window-buffer ww) buf)
           (delete-window ww))))
     'NO-MINI 'THIS-FRAME)))

;; From: emacswiki.org/emacs/download/misc-cmds.el
(defun +delete-window-maybe-kill-buffer ()
  "Delete selected window.
If no other window shows its buffer, kill the buffer too."
  (interactive)
  (let* ((selwin (selected-window))
         (buf (window-buffer selwin)))
    (delete-window selwin)
    (unless (get-buffer-window buf 'visible) (kill-buffer buf))))

(defun +replace-in-buffer (old new)
  "Replace OLD with NEW in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil)
          (matches 0))
      (while (re-search-forward old nil t)
        (replace-match new)
        (cl-incf matches))
      matches)))

(defun +clear-frenchy-ponctuations ()
  "Replace french ponctuations (like unsectable space) by regular ones."
  (interactive)
  (let ((chars
         '(("[\u00a0\u200b]" . "") ;; Non-breaking and zero-width spaces
           ;; Special spaces and quads
           ("[\u2000-\u200A\u202F\u205F\u3000]" . " ")
           ("[‘’‚’]" . "'")
           ("[“”„”«»]" . "\"")))
        (matches 0))
    (dolist (pair chars)
      (cl-incf matches (+replace-in-buffer (car pair) (cdr pair))))
    (message "Replaced %d match%s." matches (if (> matches 1) "es" ""))))

(defun +kill-region-as-paragraph ()
  "Kill (copy) region as one paragraph.
This command removes new line characters between lines."
  (interactive)
  (when (use-region-p)
    (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (let ((case-fold-search nil))
          (while (re-search-forward "\n[^\n]" nil t)
            (replace-region-contents
             (- (point) 2) (- (point) 1)
             (lambda (&optional a b) " ")))
          (kill-new (buffer-string)))))
    (deactivate-mark)))

(defun +first-line-empty-p ()
  "Return t when the first line of the buffer is empty."
  (save-excursion (goto-char (point-min))
                  (and (bolp) (eolp))))



;;; Project tweaks

(defgroup minemacs-project nil
  "MinEmacs project stuff."
  :group 'minemacs)

(defcustom +project-scan-dir-paths nil
  "A list of paths to scan and add to known projects list.
It can be a list of strings (paths) or a list of (cons \"~/path\" recursive-p) to scan directories recursively."
  :group 'minemacs-project
  :type '(repeat (choice directory (cons directory boolean))))

(defun +project-scan-for-projects (&optional dir)
  "Scan and remember projects under DIR or `+project-scan-dir-paths'."
  (interactive)
  (dolist (cons-dir (or dir +project-scan-dir-paths))
    (let* ((cons-dir (ensure-list cons-dir))
           (root-dir (car cons-dir))
           (recursive (cdr cons-dir))
           (sub-dirs (and (file-directory-p root-dir) (+directory-subdirs root-dir))))
      (dolist (dir sub-dirs)
        (project-remember-projects-under dir recursive)))))

(defun +project-add-project (dir &optional dont-ask)
  "Switch to another project at DIR.
When DIR is not detected as a project, ask to force it to be by adding a
\".project.el\" file. When DONT-ASK is non-nil, create the file without asking."
  (interactive (list (project-prompt-project-dir)))
  (project-switch-project dir)
  (when (and (not (project-current))
             (or dont-ask
                 (yes-or-no-p "Directory not detected as a project, add \".project.el\"? ")))
    (with-temp-buffer
      (write-file (expand-file-name ".project.el" dir)))))

(defun +project-forget-zombie-projects ()
  "Forget all known projects that don't exist any more.

Like `project-forget-zombie-projects', but handles remote projects differently,
it forget them only when we are sure they don't exist."
  (interactive)
  (dolist (proj (project-known-project-roots))
    (unless (or (and (file-remote-p proj nil t) (file-readable-p proj)) ; Connected remote + existent project
                (file-remote-p proj) ; Non connected remote project
                (file-exists-p proj)) ; Existent local project
      (project-forget-project proj))))

(defun +project-gdb ()
  "Invoke `gdb' in the project's root."
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (call-interactively #'gdb)))

(defun +project-list-cleanup ()
  "Forget all duplicate known projects (/home/user/proj, ~/proj)."
  (interactive)
  (let* ((projs (mapcar #'expand-file-name (project-known-project-roots)))
         (projs-dups (cl-set-difference projs (cl-remove-duplicates projs :test #'string=))))
    (mapc #'project-forget-project projs-dups)
    (project-forget-zombie-projects)
    (dolist (proj projs)
      (let ((proj-abbrev (abbreviate-file-name proj)))
        (unless (string= proj proj-abbrev)
          (project-forget-project proj)
          (project-remember-projects-under proj-abbrev))))))



;;; Systemd helpers

(defun +systemd-running-p (service)
  "Check if the systemd SERVICE is running."
  (zerop (call-process "systemctl" nil nil nil "--user" "is-active" "--quiet" service ".service")))

(defun +systemd-command (service command &optional pre-fn post-fn)
  "Call systemd with COMMAND and SERVICE."
  (when pre-fn (funcall pre-fn))
  (let ((success (zerop (call-process "systemctl" nil nil nil "--user" command service ".service"))))
    (unless success
      (user-error "[systemd]: Failed on calling '%s' on service %s.service" command service))
    (when post-fn (funcall post-fn success))
    success))

(defun +systemd-start (service &optional pre-fn post-fn)
  "Start systemd SERVICE. Optionally run PRE-FN and POST-FN."
  (+systemd-command service "start" pre-fn post-fn))

(defun +systemd-stop (service &optional pre-fn post-fn)
  "Stops the systemd SERVICE. Optionally run PRE-FN and POST-FN."
  (+systemd-command service "stop" pre-fn post-fn))



;;; Proxy
;;; =====

(defun minemacs-enable-proxy ()
  "Set *_proxy Linux environment variables from `minemacs-proxies'."
  (interactive)
  (dolist (prox minemacs-proxies)
    (let ((var (format "%s_proxy" (car prox))))
      (+log! "Set %S to %S" var (cdr prox))
      (setenv var (cdr prox)))))

(defun minemacs-disable-proxy ()
  "Unset *_proxy Linux environment variables."
  (interactive)
  (mapc #'setenv (mapcar (apply-partially #'format "%s_proxy") (mapcar #'car minemacs-proxies))))



;;; Keybinding macros
;;; =================

;; PERF+HACK: At some point, MinEmacs startup become too slow, specially when
;; initializing `general' and `evil'. After trying several configurations, I
;; figured out that deferring `general' solves the issue. However, deferring
;; `general' means that we cannot define the keybindings when loading other
;; packages, i.e. before `general' gets loaded and the MinEmacs definers (i.e.
;; `+minemacs--internal-map!', `+minemacs--internal-map-local!', ...) are made
;; available. We overcome this by defining these macros to define the
;; keybindings by wrapping the actual definition in a `with-eval-after-load'
;; block to be evaluated only after `general' gets loaded and configured and the
;; definers are ready (See `me-keybindings').
(defmacro +map! (&rest args)
  "A wrapper around `+minemacs--internal-map!'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'me-general-ready
    (+minemacs--internal-map! ,@args)))

(defmacro +map-local! (&rest args)
  "A wrapper around `+minemacs--internal-map-local!'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'me-general-ready
    (+minemacs--internal-map-local! ,@args)))

;; Wrappers around `general's VIM like definers, needs `general-evil-setup' to
;; be executed (See `me-keybindings')
(defmacro +nmap! (&rest args)
  "A wrapper around `general-nmap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'me-general-ready
    (general-nmap ,@args)))

(defmacro +vmap! (&rest args)
  "A wrapper around `general-vmap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'me-general-ready
    (general-vmap ,@args)))

(defmacro +mmap! (&rest args)
  "A wrapper around `general-mmap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'me-general-ready
    (general-mmap ,@args)))

(defmacro +imap! (&rest args)
  "A wrapper around `general-imap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'me-general-ready
    (general-imap ,@args)))

(defmacro +emap! (&rest args)
  "A wrapper around `general-emap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'me-general-ready
    (general-emap ,@args)))

(defmacro +omap! (&rest args)
  "A wrapper around `general-omap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'me-general-ready
    (general-omap ,@args)))

(defmacro +rmap! (&rest args)
  "A wrapper around `general-rmap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'me-general-ready
    (general-rmap ,@args)))

(defmacro +iemap! (&rest args)
  "A wrapper around `general-iemap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'me-general-ready
    (general-iemap ,@args)))

(defmacro +nvmap! (&rest args)
  "A wrapper around `general-nvmap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'me-general-ready
    (general-nvmap ,@args)))



;;; Data serialization

(defcustom +serialized-symbols-directory (concat minemacs-local-dir "+serialized-symbols/")
  "Default directory to store serialized symbols."
  :group 'minemacs-core
  :type 'directory)

(defun +serialize-sym (sym &optional dir filename-format)
  "Serialize SYM to DIR.
If FILENAME-FORMAT is non-nil, use it to format the file name (ex.
\"file-%s.el\"). Return the written file name, or nil if SYM is not bound."
  (when (boundp sym)
    (let ((out-file (expand-file-name
                     (format (or filename-format "%s.el") (symbol-name sym))
                     (or dir +serialized-symbols-directory))))
      (+log! "Saving `%s' to file \"%s\"" (symbol-name sym) (abbreviate-file-name out-file))
      (with-temp-buffer
        (prin1 (eval sym) (current-buffer))
        (+shutup! (write-file out-file)))
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
      (+log! "Loading `%s' from file \"%s\"" sym (abbreviate-file-name in-file))
      (with-temp-buffer
        (insert-file-contents in-file)
        (goto-char (point-min))
        (ignore-errors (setq res (read (current-buffer)))))
      (when mutate (set sym res)))
    res))



;;; Persistent & per-project scratch buffers

(defvar +scratch-default-file "__default"
  "The default file name for a project-less scratch buffer.

Will be saved in `+scratch-dir'.")

(defvar +scratch-dir (concat minemacs-local-dir "pscratch/")
  "Where to save persistent scratch buffers.")

(defvar +scratch-initial-major-mode nil
  "What major mode to start fresh scratch buffers in.

Scratch buffers preserve their last major mode, however, so this only affects
the first, fresh scratch buffer you create. This accepts:

  t           Inherits the major mode of the last buffer you had selected.
  nil         Uses `fundamental-mode'
  MAJOR-MODE  Any major mode symbol")

(defvar +scratch-buffers nil
  "A list of active scratch buffers.")

(defvar +scratch-current-project nil
  "The name of the project associated with the current scratch buffer.")
(put '+scratch-current-project 'permanent-local t)

(defvar +scratch-buffer-created-hook ()
  "The hooks to run after a scratch buffer is created.")

(defun +scratch-load-persistent-scratch-buffer (&optional project-name)
  (setq-local +scratch-current-project (or project-name +scratch-default-file))
  (let ((smart-scratch-file
         (expand-file-name (concat +scratch-current-project ".el") +scratch-dir)))
    (make-directory +scratch-dir t)
    (when (file-readable-p smart-scratch-file)
      (+log! "Reading persistent scratch from %s" smart-scratch-file)
      (cl-destructuring-bind (content point mode)
          (with-temp-buffer
            (save-excursion (insert-file-contents smart-scratch-file))
            (read (current-buffer)))
        (erase-buffer)
        (funcall mode)
        (insert content)
        (goto-char point)
        t))))

(defun +scratch-buffer (&optional dont-restore-p mode directory proj-name)
  "Return a scratchpad buffer in major MODE.

When DONT-RESTORE-P, do not load the previously saved persistent buffer. Load
persistent buffer dedicated to PROJ-NAME when provided.

When provided, set the `default-directory' to DIRECTORY."
  (let* ((buff-name (if proj-name (format "*pscratch:%s*" proj-name) "*pscratch*"))
         (pscratch-buff (get-buffer buff-name)))
    (with-current-buffer (or pscratch-buff (get-buffer-create buff-name))
      (setq-local default-directory (or directory default-directory)
                  so-long--inhibited t)
      (if dont-restore-p
          (erase-buffer)
        (unless pscratch-buff
          (+scratch-load-persistent-scratch-buffer proj-name)
          (when (and (eq major-mode 'fundamental-mode) (functionp mode))
            (funcall mode))))
      (cl-pushnew (current-buffer) +scratch-buffers)
      (+hook-once! 'window-buffer-change-functions (+scratch-persist-buffers-h))
      (+hook-once! 'server-visit-hook (+scratch-persist-buffers-h))
      (+hook-once! 'window-selection-change-functions (+scratch-persist-buffers-h))
      (add-hook 'kill-buffer-hook #'+scratch-persist-buffer-h nil 'local)
      (run-hooks '+scratch-buffer-created-hook)
      (current-buffer))))

;; Persistent scratch buffer

(defun +scratch-persist-buffer-h (&rest _)
  "Save the current buffer to `+scratch-dir'."
  (let ((content (buffer-substring-no-properties (point-min) (point-max)))
        (curr-point (point))
        (mode major-mode))
    (with-temp-file (expand-file-name (concat (or +scratch-current-project +scratch-default-file) ".el") +scratch-dir)
      (prin1 (list content curr-point mode) (current-buffer)))))

(defun +scratch-persist-buffers-h (&rest _)
  "Save all scratch buffers to `+scratch-dir'."
  (setq +scratch-buffers (cl-delete-if-not #'buffer-live-p +scratch-buffers))
  (dolist (buffer +scratch-buffers)
    (with-current-buffer buffer
      (+scratch-persist-buffer-h))))

(defun +scratch-persist-buffers-after-switch-h (&rest _)
  "Kill scratch buffers when they are no longer visible, saving them to disk."
  (unless (cl-some #'get-buffer-window +scratch-buffers)
    (mapc #'kill-buffer +scratch-buffers)
    (remove-hook '+switch-buffer-hook #'+scratch-persist-buffers-after-switch-h)))

(unless noninteractive
  (add-hook 'kill-emacs-hook #'+scratch-persist-buffers-h))

;; Commands

(defun +scratch-open-buffer (&optional arg project-p same-window-p)
  "Pop up a persistent scratch buffer.

If passed the prefix ARG, do not restore the last scratch buffer.
If PROJECT-P is non-nil, open a persistent scratch buffer associated with the
current project. When SAME-WINDOW-P is non-nil, open in the current window."
  (interactive "P")
  (let ((proj (and project-p (project-current))))
    (funcall
     (if same-window-p #'switch-to-buffer #'pop-to-buffer)
     (+scratch-buffer
      arg
      (cond ((eq +scratch-initial-major-mode t)
             (unless (or buffer-read-only ;; not a read-only buffer
                         (derived-mode-p 'special-mode) ;; not in some sort of special mode (view only)
                         (string-match-p "^ ?\\*" (buffer-name))) ;; not a hidden buffer
               major-mode))
            ((symbolp +scratch-initial-major-mode)
             +scratch-initial-major-mode))
      (and proj (project-root proj))
      (and proj (project-name proj))))))

(defun +switch-to-scratch-buffer (&optional arg project-p)
  "Like `+scratch-open-buffer', but switch to it in the current window.

If passed the prefix ARG, do not restore the last scratch buffer. If PROJECT-P,
open the persistent buffer dedicated to the current project."
  (interactive "P")
  (+scratch-open-buffer arg project-p 'same-window))

(defun +scratch-open-project-scratch-buffer (&optional arg same-window-p)
  "Opens the (persistent) project scratch buffer in a popup.

If passed the prefix ARG, do not restore the last scratch buffer. When
SAME-WINDOW-P is non-nil, open in the same window."
  (interactive "P")
  (+scratch-open-buffer arg 'project same-window-p))

(defun +scratch-switch-to-project-scratch-buffer (&optional arg)
  "Like `+scratch-open-project-scratch-buffer', but switch in the current window.

If passed the prefix ARG, do not restore the last scratch buffer."
  (interactive "P")
  (+scratch-open-project-scratch-buffer arg 'same-window))

(defun +scratch-revert-scratch-buffer ()
  "Revert scratch buffer to last persistent state."
  (interactive)
  (unless (string-match-p "^\\*pscratch" (buffer-name))
    (user-error "Not in a scratch buffer"))
  (when (+scratch-load-persistent-scratch-buffer +scratch-current-project)
    (message "Reloaded scratch buffer")))

(defun +scratch-delete-persistent-scratch-file (&optional arg)
  "Deletes a scratch buffer file in `+scratch-dir'.

If prefix ARG, delete all persistent scratches."
  (interactive)
  (if arg
      (progn
        (delete-directory +scratch-dir t)
        (message "Cleared %S" (abbreviate-file-name +scratch-dir)))
    (make-directory +scratch-dir t)
    (let ((file (read-file-name "Delete scratch file > " +scratch-dir "scratch")))
      (if (not (file-exists-p file))
          (message "%S does not exist" (abbreviate-file-name file))
        (delete-file file)
        (message "Successfully deleted %S" (abbreviate-file-name file))))))

(defun +scratch-replace-with-persistent-scratch (&optional arg project-p)
  "Replace the *scratch* buffer with a persistent one.

ARG and PROJECT-P are passed to `+scratch-open-buffer'."
  (interactive "P")
  (when-let ((buf (current-buffer))
             (s (get-buffer "*scratch*")))
    ;; Load the default persistent scratch buffer
    (+scratch-open-buffer arg project-p 'same-window)
    ;; Kill the Emacs' default scratch buffer
    (kill-buffer s)
    ;; Switch to the previous buffer, unless it has been killed (we was in *scratch*) or it is a hidden buffer
    (when (and (buffer-live-p buf) (string-match-p "^[^ ]" (buffer-name buf)))
      (switch-to-buffer buf))))

(with-eval-after-load 'project
  (setq project-switch-commands
        ;; Insert it before the last element ("Other")
        (append (butlast project-switch-commands 1)
                '((+scratch-open-project-scratch-buffer "Scratch buffer"))
                (last project-switch-commands)))
  (define-key project-prefix-map "X" #'+scratch-open-project-scratch-buffer))



;;; Font and script settings

(defcustom minemacs-fonts-plist
  '(:default
    ((:family "Iosevka Fixed Curly Slab" :height 110)
     (:family "Iosevka Comfy Fixed" :height 110)
     (:family "Iosevka Fixed Curly" :height 110)
     (:family "Iosevka Comfy Motion Fixed" :height 110)
     (:family "Iosevka" :height 110)
     (:family "Iosevka Comfy" :height 110)
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
    ("Lato"
     "Roboto"
     "Inter"
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
  '(:family :foundry :width :height :weight :slant :foreground
    :distant-foreground :background :underline :overline :strike-through :box
    :inverse-video :stipple :font :inherit :extend)
  "Arguments accepted by the `set-face-attribute' function.")

(defconst +font-spec-keywords
  '(:family :foundry :width :weight :slant :adstyle :registry :dpi :size
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
  (and font-family (member font-family (font-family-list)) t))

(defun +apply-font-or-script (script-or-face)
  "Set font for SCRIPT-OR-FACE from `minemacs-fonts-plist'."
  (catch 'done
    (dolist (font (plist-get minemacs-fonts-plist (intern (format ":%s" script-or-face))))
      (let* ((spec (+font--get-valid-args script-or-face font))
             (scale (and (plistp font) (plist-get font :scale)))
             (prependp (and (plistp font) (plist-get font :family)))
             (family (plist-get spec :family))
             (scriptp (memq script-or-face +known-scripts)))
        (when (or (not family) (+font-installed-p family))
          (if scriptp
              (set-fontset-font t script-or-face (apply #'font-spec spec) nil prependp)
            (custom-theme-set-faces 'user `(,script-or-face ((t ,spec)))))
          (when (and scale family)
            (add-to-list 'face-font-rescale-alist (cons family scale)))
          (+log! "Settinng %s `%s' to `%s'" (if scriptp "script" "face") script-or-face spec)
          (throw 'done spec))))))

;; Inspired by: github.com/seagle0128/.emacs.d/blob/master/custom-example.el
(defun +setup-fonts ()
  "Setup fonts."
  (interactive)
  (mapc #'+apply-font-or-script
        (reverse
         (mapcar (lambda (k) (intern (substring (symbol-name k) 1)))
                 (+plist-keys minemacs-fonts-plist))))
  ;; Run hooks
  (run-hooks 'minemacs-after-setup-fonts-hook))

(+add-hook! (window-setup server-after-make-frame) #'+setup-fonts)



(defun +list-external-dependencies ()
  "Show the list of declared external dependencies."
  (interactive)
  (unless (featurep 'me-external-tools) (+load minemacs-core-dir "me-external-tools.el"))
  (with-current-buffer (get-buffer-create "*external-dependencies*")
    (read-only-mode -1)
    (delete-region (point-min) (point-max))
    (insert "# External Tools
To get the maximum out of this configuration, you would need to install some
external tools, either in your development machine, docker, remote host, etc.
The presence of these programs isn't mandatory, however, for better experience,
you might need install some of these tools.\n\n")
    (let ((counter 0))
      (dolist (dep minemacs-external-dependencies)
        (insert (format "%d. [%s](%s) - %s\n"
                        (cl-incf counter)
                        (string-join (mapcar (apply-partially #'format "`%s`")
                                             (ensure-list (plist-get dep :tool)))
                                     ", ")
                        (plist-get dep :link)
                        (plist-get dep :desc)))))
    (markdown-mode)
    (read-only-mode 1)
    (pop-to-buffer (current-buffer))))



(provide 'me-lib)

;;; me-lib.el ends here
