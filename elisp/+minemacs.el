;; -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

;;;###autoload
(defmacro +error! (msg &rest vars)
  "Log error MSG and VARS using `message'."
  (when (>= minemacs-msg-level 1)
    `(apply #'message (list (concat "[MinEmacs:Error] " ,msg) ,@vars))))

;;;###autoload
(defmacro +info! (msg &rest vars)
  "Log info MSG and VARS using `message'."
  (when (>= minemacs-msg-level 2)
    `(let ((inhibit-message t))
      (apply #'message (list (concat "[MinEmacs:Info] " ,msg) ,@vars)))))

;;;###autoload
(defmacro +log! (msg &rest vars)
  "Log MSG and VARS using `message' when `minemacs-verbose' is non-nil."
  (when (>= minemacs-msg-level 3)
    `(let ((inhibit-message t))
      (apply #'message (list (concat "[MinEmacs:Log] " ,msg) ,@vars)))))

;;;###autoload
(defmacro +debug! (msg &rest vars)
  "Log error MSG and VARS using `message'."
  (when (>= minemacs-msg-level 4)
    `(let ((inhibit-message t))
      (apply #'message (list (concat "[MinEmacs:Debug] " ,msg) ,@vars)))))

;;;###autoload
(defun +emacs-features-p (&rest feats)
  "Is features FEATS are enabled in this Emacs build."
  (and (cl-every (lambda (feat) (memq feat emacs/features)) feats) t))

;;;###autoload
(defmacro +fn-inhibit-messages! (fn &optional no-message-log)
  "Add an advice around the function FN to suppress messages in echo area.
If NO-MESSAGE-LOG is non-nil, do not print any message to *Messages* buffer."
  (let ((advice-fn (make-symbol (format "+%s--inhibit-messages-a" fn))))
    `(advice-add
      ',fn :around
      (defun ,advice-fn (origfn &rest args)
       (let ((message-log-max (unless ,no-message-log message-log-max)))
        (with-temp-message (or (current-message) "")
         (+log! "Inhibiting messages of %s" ,(symbol-name fn))
         (apply origfn args)))))))

;;;###autoload
(defmacro +shutup! (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (if (not minemacs-verbose)
      `(let ((message-log-max nil))
        (with-temp-message (or (current-message) "") ,@body))
    `(progn ,@body)))

;;;###autoload
(defmacro +suppress! (&rest body)
  "Suppress new messages temporarily in the echo area while BODY is evaluated."
  (if (not minemacs-verbose)
      `(with-temp-message (or (current-message) "") ,@body)
    `(progn ,@body)))

;;;###autoload
(defmacro +cmdfy! (&rest body)
  "Convert BODY to an interactive command."
  `(lambda ()
     (interactive)
     ,@body))

;;;###autoload
(defun +set-fonts ()
  "Set Emacs' fonts from `minemacs-fonts'."
  (interactive)
  (custom-set-faces
   `(default
     ((t (:font ,(format "%s %d"
                  (or (plist-get minemacs-fonts :font-family)
                   (plist-get minemacs-default-fonts :font-family))
                  (or (plist-get minemacs-fonts :font-size)
                   (plist-get minemacs-default-fonts :font-size)))))))
   `(fixed-pitch
     ((t (:inherit (default)))))
   `(fixed-pitch-serif
     ((t (:inherit (default)))))
   `(variable-pitch
     ((t (:font ,(format "%s %d"
                  (or (plist-get minemacs-fonts :variable-pitch-font-family)
                   (plist-get minemacs-default-fonts :variable-pitch-font-family))
                  (or (plist-get minemacs-fonts :variable-pitch-font-size)
                   (plist-get minemacs-default-fonts :variable-pitch-font-size))))))))
  ;; Run hooks
  (run-hooks 'minemacs-after-set-fonts-hook))

;;;###autoload
(defun +load-theme ()
  "Load Emacs' theme from `minemacs-theme'."
  (interactive)
  (when minemacs-theme
    (+log! "Loading user theme: %s" minemacs-theme)
    ;; Fallback to built-in `tsdh-light' when `minemacs-theme' is not available.
    (unless (ignore-errors (load-theme minemacs-theme t))
      (+error! "Cannot load theme \"%s\", falling back to \"tsdh-light\"." minemacs-theme)
      (load-theme 'tsdh-light t)))
  ;; Run hooks
  (run-hooks 'minemacs-after-load-theme-hook))

;; An internal variable to keep track of the tasks
(defvar +eval-when-idle--task-num 0)
(defcustom +eval-when-idle-delay 5.0
  "The default delay (in seconds) to consider in `+eval-when-idle!' macro."
  :group 'minemacs-core
  :type 'float)

;;;###autoload
(defun +eval-when-idle (delay &rest fns)
  "Queue FNS to be processed when Emacs becomes idle."
  (let* ((task-num (cl-incf +eval-when-idle--task-num))
         (task-name (make-symbol (format "+eval-when-idle--task-%d" task-num))))
    (with-memoization (get task-name 'timer)
      (run-with-idle-timer
       delay t
       (lambda ()
         (when-let (fn (pop fns))
           (+log! "Running task %d, calling function `%s'" task-num
                  (truncate-string-to-width (format "%s" fn) 40 nil nil "…"))
           (funcall fn))
         (unless fns
           (cancel-timer (get task-name 'timer))
           (put task-name 'timer nil)))))))

;;;###autoload
(defmacro +eval-when-idle! (&rest body)
  "Evaluate BODY when Emacs becomes idle."
  (declare (indent 0))
  `(+eval-when-idle ,+eval-when-idle-delay
    (lambda ()
      ,@body)))

;;;###autoload
(defmacro +eval-when-idle-for! (delay &rest body)
  "Evaluate BODY after DELAY seconds from Emacs becoming idle."
  (declare (indent 1))
  `(+eval-when-idle ,delay
    (lambda ()
      ,@body)))

;;;###autoload
(defmacro +deferred! (&rest body)
  "Run BODY after Emacs gets loaded, a.k.a. after `minemacs-loaded'."
  `(with-eval-after-load 'minemacs-loaded
    ,@body))

;;;###autoload
(defmacro +deferred-when! (condition &rest body)
  "Like `+deferred!', with BODY executed only if CONDITION is non-nil."
  (declare (indent 1))
  `(when ,condition (+deferred! ,@body)))

;;;###autoload
(defmacro +deferred-unless! (condition &rest body)
  "Like `+deferred!', with BODY executed only if CONDITION is nil."
  (declare (indent 1))
  `(unless ,condition (+deferred! ,@body)))

;;;###autoload
(defmacro +deferred-or-immediate! (condition &rest body)
  "Like `+deferred!', with BODY deferred if CONDITION is non-nil, otherwise it acts like `progn'."
  (declare (indent 1))
  `(if ,condition (+deferred! ,@body) (progn ,@body)))

;;;###autoload
(defmacro +lazy! (&rest body)
  "Run BODY as a lazy block (see `minemacs-lazy')."
  `(with-eval-after-load 'minemacs-lazy
    (+eval-when-idle-for! 1.0
     ,@body)))

;;;###autoload
(defmacro +lazy-when! (condition &rest body)
  "Like `+lazy!', with BODY executed only if CONDITION is non-nil."
  (declare (indent 1))
  `(when ,condition (+lazy! ,@body)))

;;;###autoload
(defmacro +lazy-unless! (condition &rest body)
  "Like `+lazy!', with BODY executed only if CONDITION is nil."
  (declare (indent 1))
  `(unless ,condition (+lazy! ,@body)))

;;;###autoload
(defmacro +lazy-or-immediate! (condition &rest body)
  "Like `+lazy!', with BODY deferred if CONDITION is non nil, otherwise it acts like `progn'."
  (declare (indent 1))
  `(if ,condition (+lazy! ,@body) (progn ,@body)))

;;;###autoload
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
           (cl-loop
            for next in (cdr features)
            collect `(with-eval-after-load ',(+unquote next) ,@body))))
         ((memq feature '(:and :all))
          (dolist (next (reverse (cdr features)) (car body))
            (setq body `((with-eval-after-load ',(+unquote next) ,@body)))))
         (t `(+after-load! '(:all ,@features) ,@body)))))))

;; Adapted from: github.com/d12frosted/environment
;;;###autoload
(defmacro +hook-with-delay! (hook secs function &optional depth local)
  "Add the FUNCTION to the value of HOOK.
The FUNCTION is delayed to be evaluated in SECS once HOOK is
triggered.
DEPTH and LOCAL are passed as is to `add-hook'."
  (let* ((f-name (make-symbol (format "%s-on-%s-delayed-%ds-h" (+unquote function) (+unquote hook) secs)))
         (f-doc (format "Call `%s' in %d seconds" (symbol-name (+unquote function)) secs)))
    `(eval-when-compile
       (defun ,f-name () ,f-doc
        (run-with-idle-timer ,secs nil ,function))
       (add-hook ,hook #',f-name ,depth ,local))))

(defvar +hook-once-num 0)

;;;###autoload
(defmacro +hook-once! (hook &rest body)
  "Hook BODY in HOOK, it runs only once."
  (declare (indent 1))
  (let ((hook (+unquote hook))
        (fn-name (intern (format "+hook-once--function-%d-h" (cl-incf +hook-once-num)))))
    `(add-hook
      ',hook
      (defun ,fn-name (&rest _)
       ,(macroexp-progn body)
       (remove-hook ',hook ',fn-name)))))

;; From Doom Emacs
(defun +resolve-hook-forms (hooks)
  "Converts a list of modes into a list of hook symbols.

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
           for mode = (string-remove-suffix "-hook" (symbol-name hook))
           append
           (cl-loop for (var . val) in vars
                    collect
                    (list var val hook
                          (intern (format "minemacs--setq-%s-for-%s-h"
                                          var mode))))))

;; From Doom Emacs
;;;###autoload
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
                    ((prog1 `(lambda (&rest _) ,@(cons next rest))
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
;;;###autoload
(defmacro +remove-hook! (hooks &rest rest)
  "A convenience macro for removing N functions from M hooks.

Takes the same arguments as `add-hook!'.

If N and M = 1, there's no benefit to using this macro over `remove-hook'.

\(fn HOOKS [:append :local] FUNCTIONS)"
  (declare (indent defun) (debug t))
  `(+add-hook! ,hooks :remove ,@rest))

;; From Doom Emacs
;;;###autoload
(defmacro +setq-hook! (hooks &rest var-vals)
  "Sets buffer-local variables on HOOKS.

\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (var val hook fn) in (+setq-hook-fns hooks var-vals)
            collect `(defun ,fn (&rest _)
                      ,(format "%s = %s" var (pp-to-string val))
                      (setq-local ,var ,val))
            collect `(add-hook ',hook #',fn -90))))

;; From Doom Emacs
;;;###autoload
(defmacro +unsetq-hook! (hooks &rest vars)
  "Unbind setq hooks on HOOKS for VARS.

\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (_var _val hook fn)
            in (+setq-hook-fns hooks vars 'singles)
            collect `(remove-hook ',hook #',fn))))

;; Adapted from: Doom Emacs
;;;###autoload
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

;;;###autoload
(defun +env-save ()
  "Load environment variables from shell and save them to `+env-file'."
  (interactive)
  (with-temp-buffer
    (insert ";; -*- mode: emacs-lisp; no-byte-compile: t; no-native-compile: t; -*-\n\n")
    (let ((env-vars
           (mapcar ; Get environment variables from shell into an alist
            (lambda (l) (let ((s (string-split l "="))) (cons (car s) (string-join (cdr s) "="))))
            (string-lines (shell-command-to-string "env") "="))))
      ;; Special treatment for the "PATH" variable, save it to `exec-path'
      (when-let ((path (alist-get "PATH" env-vars nil nil #'string=)))
        (insert "\n;; Adding PATH content to `exec-path'\n"
                (format "(setq exec-path (delete-dups (append exec-path '%s)))\n\n"
                        (mapcar (lambda (s) (concat "\"" s "\"")) (parse-colon-path path)))))
      ;; Save the environment variables to `process-environment' using `setenv'
      (insert ";; Adding the rest of the environment variables\n")
      (dolist (env-var env-vars)
        (unless (cl-some (+apply-partially-right #'string-match-p (car env-var)) +env-deny-vars)
          (insert (format "(setenv \"%s\" \"%s\")\n" (car env-var) (cdr env-var))))))
    (write-file +env-file)))

;;;###autoload
(defun +env-load ()
  "Load environment variables from `+env-file'."
  (interactive)
  (unless (file-exists-p +env-file) (+env-save))
  (+load +env-file))

;;;###autoload
(defun +ignore-root (&rest roots)
  "Add ROOTS to ignored projects, recentf, etc."
  (dolist (root roots)
    (with-eval-after-load 'recentf
      (add-to-list 'recentf-exclude root))))

;;;###autoload
(defun +package-disabled-p (package)
  "Is package PACKAGE disabled in `minemacs-disabled-packages'."
  (and (memq package (apply #'append (mapcar #'ensure-list minemacs-disabled-packages))) t))

;;;###autoload
(defun minemacs-run-build-functions (&optional dont-ask-p)
  "Run all build functions in `minemacs-build-functions'."
  (interactive "P")
  (dolist (fn minemacs-build-functions)
    (message "MinEmacs: Running `%s'" fn)
    (if dont-ask-p
        ;; Do not ask before installing
        (cl-letf (((symbol-function 'yes-or-no-p) #'always)
                  ((symbol-function 'y-or-n-p) #'always))
          (funcall-interactively fn))
      (funcall-interactively fn))))

;;;###autoload
(defun minemacs-update ()
  "Update MinEmacs packages."
  (interactive)
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

  ;; Run `straight's update cycle, taking into account the explicitly pinned
  ;; packages versions.
  (message "[MinEmacs]: Pulling packages")
  (straight-x-pull-all)
  (message "[MinEmacs]: Freezing packages")
  (straight-x-freeze-versions)
  (message "[MinEmacs]: Rebuilding packages")
  (straight-rebuild-all)

  ;; Runn package-specific build functions (ex: `pdf-tools-install')
  (message "[MinEmacs]: Running additional package-specific build functions")
  (minemacs-run-build-functions 'dont-ask))

;;; +minemacs.el ends here
