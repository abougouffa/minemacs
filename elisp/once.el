;;; once.el --- Extra init.el deferred evaluation utilties -*- lexical-binding: t; -*-

;; Author: Fox Kiester <noctuid@pir-hana.cafe>
;; URL: https://github.com/emacs-magus/once
;; Created: April 14, 2022
;; Keywords: convenience dotemacs startup config
;; Package-Requires: ((emacs "26.1"))
;; Version: 0.1.0

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Once.el provides extra deferred evaluation utilities that have the common
;; theme of running some code one time once some condition is met.

;; The primary provided function `once' can run code the first time some hook
;; runs, some function runs, some variable changes, or some package is loaded.
;; It additionally allows providing more specific conditions in each case
;; (e.g. only run the code when x hook runs if y condition is also met) which
;; allows handling more complex situations.  The primary use case is to be able
;; to more easily specify when packages should be loaded (e.g. load the
;; magit-todos package and activate `magit-todos-mode' when calling
;; `magit-status' for the first time).  For concrete examples, see the full
;; documentation.

;; Once.el additionally provides `eval-after-load' alternatives (see the full
;; documentation for an explanation of the differences) and some other utilities
;; like `once-require-incrementally', which is the equivalent of Doom's
;; `:defer-incrementally' for anyone familiar with it.  In combination with
;; the other "emacs-magus" packages (especially satch.el), once.el may be useful
;; for anyone coming from Doom Emacs or some other distribution/starter kit who
;; wants some of their helpful init.el configuration utilities without having to
;; copy the code.  The utilities provided by once.el are also more
;; generic/flexible than Doom's :after-call, transient advice/hooks, etc.

;; For more information see the info manual or README in the online repository.

;;; Code:
(require 'cl-lib)
(require 'rx)

;; * Settings
(defgroup once nil
  "Provides extra deferred evaluation init.el utilities."
  :group 'convenience)

(defcustom once-shorthand nil
  "Whether to allow shorthand for `once-x-call' conditions.
When shorthand is enabled, you do not need to specify :hooks, :before (and other
advice keywords), or :packages/:files.  Instead, symbols that end in \"-hook\"
or \"-functions\" will be inferred to be hooks.  Other symbols will be inferred
to be functions to advise :before.  Strings will be inferred to be
files.  Other keywords must come at the end (:check and
:initial-check).

This will also allow using a single symbol or string as the condition.

For example:
\(once #\\='foo ...)
is the same as
\(once (list :before #\\='foo) ...)

By setting this variable, you confirm that you understand how the inference
works and what its limitations are (e.g. you cannot use a feature symbol but
must use a string for a file instead: \"magit\" not \\='magit)."
  :type 'boolean)

;; * Helpers
(defmacro once--ensure-lists (&rest vars)
  "Ensure that all variables in VARS are lists if they are not already.
If any variable is a lambda, it will not be considered to be a list.  If a var
is nil, it will be set to (list nil)."
  `(progn
     ,@(mapcar (lambda (var)
                 `(unless (and ,var
                               (listp ,var)
                               ;; lambdas are lists
                               (not (functionp ,var)))
                    (setq ,var (list ,var))))
               vars)))

(defvar once--counter 0
  "Counter to use to prevent name clashes for automatically named functions.")

(defun once--unique-count (&optional expand-time)
  "Return a unique once.el counter.
If EXPAND-TIME is true, prefix it with \"e\".  Since this will end up being used
as part of a function name, we cannot use an uninterned symbol to prevent
collisions."
  (if expand-time
      (format "e%s" (cl-incf once--counter))
    (format "%s" (cl-incf once--counter))))

;; * `eval-after-load' Utilities
(defun once--after-load-function (regexp-or-feature function)
  "Return a function meant to be added to `after-load-alist'.
The function is meant to be added to `after-load-alist' for REGEXP-OR-FEATURE
and will run FUNCTION.  If REGEXP-OR-FEATURE is a not symbol, this will return
FUNCTION as-is.  Otherwise, it will return a function that temporarily adds
itself to `after-load-functions' (to ensure it runs after the load in case
`provide' is called early).

This is taken from `eval-after-load'."
  (if (not (symbolp regexp-or-feature))
      function
    ;; For features, the after-load-alist elements get run when
    ;; `provide' is called rather than at the end of the file.
    ;; So add an indirection to make sure that `function' is really run
    ;; "after-load" in case the provide call happens early.
    (lambda ()
      (if (not load-file-name)
          ;; Not being provided from a file, run function right now.
          (funcall function)
        (let ((lfn load-file-name)
              ;; Don't use letrec, because equal (in
              ;; add/remove-hook) could get trapped in a cycle
              ;; (bug#46326).
              (fun (make-symbol "once-eval-after-load-helper")))
          (fset fun (lambda (file)
                      (when (equal file lfn)
                        (remove-hook 'after-load-functions fun)
                        (funcall function))))
          (add-hook 'after-load-functions fun 'append))))))

(defun once--named-after-load-function (regexp-or-feature function)
  "Return a named function meant to be added to `after-load-alist'.
Pass REGEXP-OR-FEATURE and FUNCTION to `once-after-load-function', give the
result an automatically generated name, and then return the function symbol."
  (let ((name (intern (format "once--%s-after-%s-run-%s"
                              (once--unique-count)
                              regexp-or-feature
                              (if (symbolp function)
                                  function
                                "lambda")))))
    (defalias name (once--after-load-function regexp-or-feature function))
    name))

(defun once--after-load-remove (regexp-or-feature function)
  "For REGEXP-OR-FEATURE in `after-load-alist', remove FUNCTION as a value.
If the value is now empty, completely remove the entry."
  (let ((new-val (remove function (alist-get regexp-or-feature after-load-alist
                                             nil nil #'equal))))
    (setf (alist-get regexp-or-feature after-load-alist nil 'remove #'equal)
          new-val)))

(defun once--file-to-regexp-or-feature (file)
  "Return a regexp or feature for FILE.
If FILE is a string, return a regexp to find it in `load-history'.  Otherwise
return FILE."
  (if (stringp file)
      (purecopy (load-history-regexp file))
    file))

(defun once--regexp-or-feature-loaded-p (regexp-or-feature)
  "Return whether REGEXP-OR-FEATURE has already been loaded."
  (if (stringp regexp-or-feature)
      (load-history-filename-element regexp-or-feature)
    (featurep regexp-or-feature)))

(defun once--file-loaded-p (file)
  "Return whether FILE has already been loaded."
  (if (stringp file)
      (load-history-filename-element (purecopy (load-history-regexp file)))
    (featurep file)))

(defun once--eval-after-load (file form &optional transient)
  "Arrange that if FILE is loaded, FORM will be run immediately afterwards.
This is `eval-after-load' with some changes.  If FILE has already been loaded,
run FORM now and do not add FORM to `after-load-alist' for that file.
Otherwise, add the FORM to `after-load-alist'.  If TRANSIENT is non-nil, remove
it from `after-load-alist' once it runs.

When delaying FORM, return the constructed delay function.

See `eval-after-load' for more information."
  (declare (indent 1)
           (compiler-macro
            (lambda (whole)
              (if (eq 'quote (car-safe form))
                  ;; Quote with lambda so the compiler can look inside.
                  `(once--eval-after-load ,file (lambda () ,(nth 1 form))
                                          ,transient)
                whole))))
  (let ((regexp-or-feature (once--file-to-regexp-or-feature file))
        (func
         (if (functionp form) form
           ;; Try to use the "current" lexical/dynamic mode for `form'.
           (eval `(lambda () ,form) lexical-binding))))
    (if (once--regexp-or-feature-loaded-p regexp-or-feature)
        ;; run now and don't add to `after-load-alist'
        (funcall func)
      ;; otherwise add to `after-load-alist'
      (let ((delayed-func (once--named-after-load-function regexp-or-feature
                                                           func))
            (elt (assoc regexp-or-feature after-load-alist)))
        (unless elt
          (setq elt (list regexp-or-feature))
          (push elt after-load-alist))
        ;; Add FORM to the element unless it's already there.
        (unless (member delayed-func (cdr elt))
          (nconc elt (list delayed-func)))
        (when transient
          (advice-add
           delayed-func
           :after
           (lambda (&rest _)
             (once--after-load-remove regexp-or-feature delayed-func))))
        delayed-func))))

;;;###autoload
(defun once-eval-after-load (file form)
  "Like `eval-after-load' but don't always add to `after-load-alist'.
When FILE has already been loaded, execute FORM immediately without adding it to
`after-load-alist'.  Otherwise add it to `after-load-alist' but remove the FORM
from `after-load-alist' after it runs.  See `eval-after-load' for more
information."
  (once--eval-after-load file form t))

;;;###autoload
(defalias 'once-after-load #'once-eval-after-load)

;;;###autoload
(defmacro once-with-eval-after-load (file &rest body)
  "Like `with-eval-after-load' but don't always add to `after-load-alist'.
When FILE has already been loaded, execute BODY immediately without adding it to
`after-load-alist'.  Otherwise add it to `after-load-alist' but remove the FORM
from `after-load-alist' after it runs.  See `eval-after-load' for more
information."
  (declare (indent 1) (debug (form def-body)))
  `(once-eval-after-load ,file (lambda () ,@body)))

;;;###autoload
(defalias 'once-with #'once-with-eval-after-load)

;; * Run Once After Condition
(defun once--make-functions-transient (hook-pairs advice-pairs package-pairs
                                                  variable-pairs)
  "Advise functions to remove themselves once any of them run.

HOOK-PAIRS should be in the format:
\('hook-name <hook function>)

ADVICE-PAIRS should be in the format:
\('advised-symbol <advice function>)

PACKAGE-PAIRS should be in the format:
\(<regexp or feature symbol> <function>)

VARIABLE-PAIRS should be in the format:
\(<variable> <function>)

The difference from sharing a single transient function between hooks and advice
is that this allows different functions with specific checks (e.g. based on
arguments passed to an advised function)."
  (let ((name (intern (format "once--%s-remove-hooks-advice"
                              ;; protect against collision
                              (once--unique-count))))
        (all-functions
         (mapcar #'cadr
                 (append hook-pairs advice-pairs package-pairs variable-pairs)))
        (hook-names (mapcar #'car hook-pairs))
        (advised-symbol-names (mapcar #'car advice-pairs))
        (package-names (mapcar #'car package-pairs))
        (variable-names (mapcar #'car variable-pairs)))
    (defalias name
      (lambda (oldfun &rest args)
        (let ((result (apply oldfun args)))
          ;; only remove when returns non-nil
          (when result
            (cl-loop for (hook fun) in hook-pairs
                     do (remove-hook hook fun))
            (cl-loop for (advised-symbol fun) in advice-pairs
                     do (advice-remove advised-symbol fun))
            (cl-loop for (package _ delay-fun) in package-pairs
                     do (progn
                          (once--after-load-remove
                           (once--file-to-regexp-or-feature package)
                           delay-fun)
                          (fmakunbound delay-fun)))
            (cl-loop for (variable fun) in variable-pairs
                     do (remove-variable-watcher variable fun))
            (dolist (function all-functions)
              (advice-remove function name)
              (fmakunbound function))
            (fmakunbound name))))
      (format "Remove functions from other hooks/functions.
Remove an equivalent function from these to ensure it only runs once:
Hooks - %s
Functions - %s
Variables - %s
Files/Features - %s"
              (or hook-names "None")
              (or advised-symbol-names "None")
              (or variable-names "None")
              (or package-names "None")))
    (dolist (function all-functions)
      (advice-add function :around name))))

(defun once--make-conditional-function (function specific-check general-check)
  "Return function to run  FUNCTION when SPECIFIC-CHECK and GENERAL-CHECK pass.
SPECIFIC-CHECK is a check specific to the hook or function this function will be
added to.  This means any arguments passed to the new function will be passed to
SPECIFIC-CHECK.  GENERAL-CHECK and FUNCTION will be passed no arguments.

If SPECIFIC-CHECK or GENERAL-CHECK is nil, the check will be
skipped/automatically succeed.

The returned function will return t if both checks succeeds.  Otherwise it will
return nil."
  (let ((name (intern (format "once--%s-run-%s-conditionally"
                              ;; protect against collision
                              (once--unique-count)
                              (if (symbolp function)
                                  (symbol-name function)
                                "lambda"))))
        (specific-check (or specific-check (lambda (&rest _) t)))
        (general-check (or general-check (lambda () t))))
    (defalias name
      `(lambda (&rest args)
         ;; put docstring in lambda so it will still show even when the function
         ;; is advised
         ,(format "Call a function conditionally if checks return non-nil.
Specific check: %s
General check: %s
Function to run: %s"
                  (if (and specific-check (symbolp specific-check))
                      (format "`%s'" specific-check)
                    (format "%S" specific-check))
                  (if (and general-check (symbolp general-check))
                      (format "`%s'" general-check)
                    (format "%S" general-check))
                  (if (and function (symbolp function))
                      (format "`%s'" function)
                    (format "%S" function)))
         (when (and (apply #',specific-check args)
                    (funcall #',general-check))
           (funcall #',function)
           t)))
    name))

(defun once--call-later (function hooks advise-symbols packages variables
                                  &optional check)
  "Call FUNCTION once later when some specified condition is met.
Valid conditions are HOOKS loading, ADVISE-SYMBOLS being called, VARIABLES being
set, or PACKAGES being loaded.

If specified, CHECK should be a function that will return whether to run
FUNCTION when any hook or advised symbol runs.  It should take no arguments.

FUNCTION will only run once.  Once it runs, all added functions will be removed
from each hook and advised symbol.

HOOKS should be a list with each item in the form:
\(<hook symbol> <local-check function or nil>)
e.g.
\(\\='some-hook (lambda (&rest _hook-args) (foo-check)))

ADVISE-SYMBOLS should be a list with each item in the form:
\(<where> <advise function symbol> <optional local-check>)
e.g.
\(:before \\='some-symbol (lambda (& _function-args) (foo-check)))

FILES should be a list with each item in the form:
\(<regexp or feature symbol> <local-check function or nil>)
e.g.
\(\\='some-package (lambda () (foo-check)))

VARIABLES should be a list with each item in the form:
\(<variable> <local-check function or nil>)
e.g.
\(\\='some-variable (lambda (_symbol _newval _operation _where) (foo-check)))"
  (let ((hook-pairs
         (cl-loop for (hook local-check) in hooks
                  collect (let ((maybe-function
                                 (once--make-conditional-function
                                  function
                                  local-check
                                  check)))
                            (add-hook hook maybe-function)
                            (list hook maybe-function))))
        (advice-pairs
         (cl-loop for (where advise-symbol local-check) in advise-symbols
                  collect
                  (let ((maybe-function (once--make-conditional-function
                                         function
                                         local-check
                                         check)))
                    (advice-add advise-symbol where maybe-function)
                    (list advise-symbol maybe-function))))
        (package-pairs
         (cl-loop for (package local-check) in packages
                  unless (once--file-loaded-p package)
                  collect
                  (let ((maybe-function (once--make-conditional-function
                                         function
                                         local-check
                                         check)))
                    (list package
                          ;; function to advise to determine if checks
                          ;; succeed/whether teardown should happen
                          maybe-function
                          ;; function to remove from `after-load-alist' during
                          ;; teardown
                          (once--eval-after-load package
                            maybe-function)))))
        (variable-pairs
         (cl-loop for (variable local-check) in variables
                  collect
                  (let ((maybe-function (once--make-conditional-function
                                         function
                                         local-check
                                         check)))
                    (add-variable-watcher variable maybe-function)
                    (list variable maybe-function)))))
    (once--make-functions-transient hook-pairs advice-pairs package-pairs
                                    variable-pairs)))

(defun once--call-now-or-later (function hooks advise-symbols packages variables
                                         &optional initial-check check)
  "Run FUNCTION once now or later.
INITIAL-CHECK and CHECK should be functions that take no arguments and return
non-nil if FUNCTION should run now.  If any PACKAGES have already been loaded,
run FUNCTION now.  When delaying FUNCTION, run it the first time a hook from
HOOKS triggers, a symbol in ADVISE-SYMBOLS is called, or a package in PACKAGES
loads."
  (let ((check (or initial-check check)))
    (if (if check
            (funcall check)
          (cl-some #'once--file-loaded-p (mapcar #'car packages)))
        (funcall function)
      (once--call-later function hooks advise-symbols packages variables check))))

(defun once--condition-item-to-list (item)
  "Return ITEM as as (list ITEM nil) if it is not already a list.
If it is already a list, just return ITEM."
  (if (listp item)
      item
    (list item nil)))

(defvar once--shorthand-hook-regexp (rx (or "-hook" "-functions") eol)
  "Regexp for hook symbol names by when `once-shorthand' is non-nil.")

(defun once--pre-parse-shorthand (condition)
  "Given a `once-shorthand' CONDITION, return one with the full syntax."
  (once--ensure-lists condition)
  (let (hooks
        advise-symbols
        packages)
    (while (and (car condition)
                (not (keywordp (car condition))))
      (let ((item (pop condition)))
        (cond ((and (symbolp item)
                    (string-match-p once--shorthand-hook-regexp
                                    (symbol-name item)))
               (push item hooks))
              ((symbolp item)
               (push item advise-symbols))
              ((stringp item)
               (push item packages)))))
    (nconc condition
           (when hooks
             (cons :hooks hooks))
           (when advise-symbols
             (cons :before advise-symbols))
           (when packages
             (cons :packages packages)))))

(defun once--parse-condition (condition)
  "Parse CONDITION into a partial argument list for `once--call-now-or-later'."
  (let (current-key
        current-advice-list
        hooks
        advise-symbols
        packages
        variables
        initial-check
        check)
    (when once-shorthand
      (setq condition (once--pre-parse-shorthand condition)))
    (cl-loop for item in condition
             do (cond ((keywordp item)
                       (when current-advice-list
                         (setq advise-symbols
                               (nconc advise-symbols current-advice-list))
                         (setq current-advice-list nil))
                       (setq current-key item))
                      (t
                       (cl-case current-key
                         (:hooks (push (once--condition-item-to-list item)
                                       hooks))
                         ((:packages :files)
                          (push (once--condition-item-to-list item)
                                packages))
                         ((:variables :vars)
                          (push (once--condition-item-to-list item)
                                variables))
                         (:check (setq check item))
                         (:initial-check (setq initial-check item))
                         (t
                          (push (cons current-key
                                      (once--condition-item-to-list item))
                                current-advice-list))))))
    (when current-advice-list
      (setq advise-symbols (nconc advise-symbols current-advice-list)))
    (list hooks advise-symbols packages variables initial-check check)))

;;;###autoload
(defun once-x-call (condition &rest functions)
  "When CONDITION is first met, call FUNCTIONS once.

The \"once\" has two meanings:
- Run something once some condition is met (hook OR advice with optional extra
  checks)
- Run it only once (unlike `eval-after-load')

This is inspired by `evil-delay', Doom's :after-call, Doom's `defer-until!',
etc.  It can be thought of as a combination of transient hooks, advice, and
`eval-after-load'.  It aims to be both very generic but to also provide more
convenient syntax for common cases.  If you don't need a combination of these
conditions, you can alternatively use `satch-add-hook' (from satch.el),
`satch-advice-add', or `once-with' instead.

FUNCTIONS should be a single function or a list of functions.  FUNCTIONS will
only run once.  This function will add FUNCTIONS to any specified hooks or as
advice to any specified functions, but it will remove all advice/hook additions
the first time FUNCTIONS run to prevent multiple runs.

Unlike `satch-add-hook' and `satch-advice-add' (from satch.el), all FUNCTIONS
should take no arguments.

CONDITION should be a condition in the following format:
\(list :hooks arg1 arg2... :before arg1 arg2... :check (lambda () ...) ...)

Here are the available CONDITION keywords:

- :check - an additional check to determine whether to run FUNCTIONS.  This will
  be used initially to determine whether to add any advice or to any hooks.  If
  the check returns non-nil, FUNCTIONS will be run immediately.  Otherwise, the
  advice/hook additions will be made, and FUNCTIONS will run the first time the
  check succeeds when an advised function or hook triggers.
- :initial-check - an alternate check to determine whether to run FUNCTIONS
  before adding any advice or to any hooks.  When both :check and :initial-check
  are specified, :initial-check will be used only before adding advice or adding
  to hooks, and :check will only be used when the advised function or hook
  triggers.
- :hooks - list of hooks that can trigger running FUNCTIONS
- :packages or :files - list of files/features (i.e. valid arguments to
  `eval-after-load') that can trigger running FUNCTIONS on load.  Unlike :after
  or Doom's after!, `once-x-call' does not support any sort of complex
  \"and\"/\"or\" rules for packages.  I have yet to encounter a situation where
  these are actually necessary.  Any of the specified files/packages loading can
  trigger FUNCTIONS.
- :variables or :vars - list of variables that can trigger running FUNCTIONS
  when set (using `add-variable-watcher')
- any advice WHERE position (e.g. :before or :after) - list of functions to
  advise that can trigger running FUNCTIONS

You must specify at least one of :hooks, :packages/:files, :variables, or the
advice keywords.

If you want to potentially run FUNCTIONS immediately, you must specify
:initial-check and/or :check.  The only exception is if you specify :packages.
If there are no checks and any of the specified files/features has loaded,
FUNCTIONS will be run immediately.  On the other hand, if :check is specified
and fails initially, the code will always be delayed even if one of the
files/features has already loaded.  In that case, some other method (a different
package load or a hook or advice) will have to trigger later when the :check
returns non-nil for FUNCTIONS to run.

If you specify :check but do not want FUNCTIONS to run immediately if the check
passes, you should specify :initial-check as (lambda () nil).

Arguments can specify a \"local check\" that only applies to a specific hook,
for example, by specifying a list like (<hook> <specific-check>) instead of a
single symbol.  For example:
\(list
 :hooks
 (list \\='after-load-functions (lambda (_load-file) (boundp \\='some-symbol))))

Unlike the :check and :initial-check functions, which take no arguments, a local
check function will be passed whatever arguments are given for the hook or
advice.  For :variables, a local check function will be passed <symbol newval
operation where> like the watch-function for `add-variable-watcher'.  If your
local check does not need to use any given arguments, specify (&rest _).

Packages can also specify a local check, but it will be passed no arguments, so
this may not often be useful.

Here is a phony example of what a `once-x-call' invocation looks like (you
would never actually use this condition):
\(once-x-call (list :hooks \\='pre-command-hook-hook \\='another-hook
                   :before \\='after-find-file
                   :packages \\='evil
                   :initial-check (lambda () (and (bar) (foo)))
                   :check (lambda () (foo)))
             #\\='some-mode)

If you set `once-shorthand' to non-nil, you can also use a more brief
condition syntax.  See its documentation for more information.

For real examples, see the README or specific once \"x\" utilities like
`once-gui' and `once-buffer'."
  (declare (indent 1))
  (let ((parsed-condition (once--parse-condition condition)))
    (dolist (fun functions)
      (apply #'once--call-now-or-later fun parsed-condition))))

;;;###autoload
(defmacro once (condition &rest body)
  "When CONDITION is met for the first time, execute BODY.
If the first item is BODY is anything that could be a function, it will be
considered to be a list of functions:
\(once condition #\\='foo \\='bar some-func-in-var (lambda ()))

Otherwise, if the first item is in the form (fun arg1), it will be considered to
be a function body:
 (once <condition>
   (foo)
   (bar)
   (baz))

See `once-x-call' for more information, including how to specify CONDITION."
  (declare (indent 1) (debug (form &or [body def-body])))
  (if (or (symbolp (car body))
          (and (listp (car body))
               (memq (caar body) '(lambda function quote))))
      `(once-x-call ,condition ,@body)
    `(once-x-call ,condition (lambda () ,@body))))

(defun once-x-require (condition &rest packages)
  "Once CONDITION is met the first time, require PACKAGES."
  (declare (indent 1))
  (let* ((package-strings (mapcar (lambda (x) (format "%s" x))
                                  packages))
         (require-fun (intern (concat "once-require-"
                                      (string-join package-strings "-")))))
    (defalias
      require-fun
      (lambda ()
        (dolist (package packages)
          (require package)))
      (format "Require %s." (string-join package-strings ", ")))
    (once-x-call condition require-fun)))

(provide 'once)
;; LocalWords: arg args satch el uninterned init magit newval
;;; once.el ends here
