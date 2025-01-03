;;; satch.el --- Satchel of init.el configuration utilities -*- lexical-binding: t; -*-

;; Author: Fox Kiester <https://github.com/noctuid>
;; URL: https://github.com/emacs-magus/satch.el
;; Created: April 14, 2022
;; Keywords: convenience dotemacs startup config
;; Package-Requires: ((emacs "25.1"))
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
;; Satchel of init.el configuration utilities (general.el spinoff).  It may be
;; useful for those who want some of the helpful utilities from Doom Emacs or
;; other distributions/starter kits without having to copy their code.
;;
;; Includes:
;; - Utilities related to settings, hooks, and advice
;; - Utilities related to deferring loading of code and packages (overlaps with
;;   hooks/advice)
;; - Extra use-package keywords for these utilities
;; Does not include:
;; - Data structure or general utilities (like cl-lib or dash)
;; - Key definition helpers (see general.el and eventually familiar.el)
;; - Use-package re-implementation

;; For more information see the README in the online repository.

;;; Code:
(require 'cl-lib)
(require 'rx)

;; * Settings
(defgroup satch nil
  "Provides non-keybinding-related init.el configuration utilities."
  :group 'convenience
  :prefix "satch-")

;; * Helpers
(defun satch--remove-keyword-args (rest)
  "Remove all keyword arguments from the list REST.
Return a list of the altered REST list and a list of the removed keyword
arguments.  The order of arguments will be preserved.  Note that the length of
REST does not need to be even (i.e. there can be an odd number of positional
arguments)."
  (let (args
        kargs)
    (while rest
      (cond ((keywordp (car rest))
             (push (pop rest) kargs)
             (push (pop rest) kargs))
            (t
             (push (pop rest) args))))
    (list (nreverse args) (nreverse kargs))))

(defmacro satch--ensure-lists (&rest vars)
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

(defvar satch--counter 0
  "Counter to use to prevent name clashes for automatically named functions.")

(defun satch--unique-count (&optional expand-time)
  "Return a unique satchel counter.
If EXPAND-TIME is true, prefix it with \"e\".  Since this will end up being used
as part of a function name, we cannot use an uninterned symbol to prevent
collisions."
  (if expand-time
      (format "e%s" (cl-incf satch--counter))
    (format "%s" (cl-incf satch--counter))))

(defun satch--sexp-symbol (sexp &optional format-string)
  "Return a symbol generated by altering SEXP.
`format' with FORMAT-STRING, strip parens, and convert spaces to hyphens.  The
FORMAT-STRING defaults to \"satch-%s\"."
  (intern (replace-regexp-in-string
           (rx space)
           "-"
           (replace-regexp-in-string
            (rx (any "(" ")" "\n" ))
            ""
            (format (or format-string "satch-%s") sexp)))))

;; * Setting/Variable Utilities
(defmacro satch--set (evalp fallback-setter &rest settings)
  "Helper function to implement `satch-set', `satch-setq', etc.
If `evalp' is non-nil, evaluate variable positions. Otherwise quote them.  If
there is no custom-set property for the value, fallback to FALLBACK-SETTER.
SETTINGS should be a list of variable to value pairs.

In the future, this will automatically record user SETTINGS using annalist.el."
  (unless (zerop (mod (length settings) 2))
    (error "SETTINGS must have an even number of variable/value members"))
  `(progn
     ,@(cl-loop for (var val) on settings by #'cddr
                if (or evalp (symbolp var))
                collect
                (if evalp
                    (let ((varsym (cl-gensym "satch-")))
                      `(let ((,varsym ,var))
                         (funcall (or (get ,varsym 'custom-set) ,fallback-setter)
                                  ,varsym
                                  ,val)))
                  `(funcall (or (get ',var 'custom-set) ,fallback-setter)
                            ',var
                            ,val))
                else do (error "Attempting to set a non-symbol: %s" var))))

;;;###autoload
(defmacro satch-setq (&rest settings)
  "A stripped-down `customize-set-variable' with the syntax of `setq'.
Like `setq', multiple variables can be set at once; SETTINGS should consist of
variable to value pairs.

Some variables have a custom setter (specified with `defcustom' and :set) that
is used to run code necessary for changes to take effect (e.g.
`auto-revert-interval').  If a package has already been loaded, and the user
uses `setq' to set one of these variables, the :set code will not run (e.g. in
the case of `auto-revert-interval', the timer will not be updated).  Like with
`customize-set-variable', `satch-setq' will use the custom :set setter when it
exists.  If the package defining the variable has not yet been loaded, the
custom setter will not be known, but it will still be run upon loading the
package.

Unlike `customize-set-variable', `satch-setq' does not attempt to load any
dependencies for the variable and does not support giving variables
comments (which makes it 10-100x faster, though this generally shouldn't
matter).  It also falls back to `set' instead of `set-default', so that like
`setq' it will change the local value of a buffer-local variable instead of the
default value.  See `satch-setq-default' for an equivalent that falls back to
`set-default'.

In the future, this will automatically record user SETTINGS using annalist.el."
  (declare (debug setq))
  `(satch--set nil #'set ,@settings))

;;;###autoload
(defmacro satch-set (&rest settings)
  "Like `satch-setq' but evaluate variable positions like `set'.
In the future, this will automatically record user SETTINGS using annalist.el."
  `(satch--set t #'set ,@settings))

;;;###autoload
(defmacro satch-setq-default (&rest settings)
  "Like `satch-setq' but fall back to `set-default' if no custom setter.
In the future, this will automatically record user SETTINGS using annalist.el."
  `(satch--set nil #'set-default ,@settings))

;;;###autoload
(defmacro satch-setq-local (&rest settings)
  "Like `satch-set' but make all variables in SETTINGS buffer-local.
In the future, this will automatically record user settings using annalist.el."
  `(satch-set ,@(cl-loop for (var val) on settings by #'cddr
                         collect `(make-local-variable ',var)
                         and collect val)))

;;;###autoload
(defmacro satch-pushnew (x place &rest keys)
  "Call `cl-pushnew' with X, PLACE, and KEYS.
:test defaults to `equal'.  If PLACE has a a custom-set function, call it
afterwards with PLACE and its new value.

In the future, this will automatically record user settings using annalist.el."
  (declare (debug
            (form place &rest
                  &or [[&or ":test" ":test-not" ":key"] function-form]
                  [keywordp form])))
  (let ((custom-setter (cl-gensym "satch-")))
    `(progn
       (cl-pushnew ,x ,place ,@keys :test #'equal)
       (when-let ((,custom-setter (get ,place 'custom-set)))
         (funcall ,custom-setter ',place ,place)))))

(cl-defmacro satch-shove (place values &rest keys)
  "Like `satch-pushnew' but PLACE is first followed by a list of VALUES.
This is also similiar to `nconc', but it will not add duplicates to PLACE.  KEYS
will be passed to `cl-pushnew'. :test defaults to `equal'.  If PLACE has a a
custom-set function, call it afterwards with PLACE and its new value.

In the future, this will automatically record user settings using annalist.el."
  (let ((custom-setter (cl-gensym "satch-setter-"))
        (val (cl-gensym "satch-value-")))
    `(progn
       (dolist (,val ,values)
         (cl-pushnew ,val ,place ,@keys :test #'equal))
       (when-let ((,custom-setter (get ',place 'custom-set)))
         (funcall ,custom-setter ',place ,place)))))

;; * Hook and Advice Utilities
;; ** Hook Utilities
;; using a function instead of a macro in order to keep the original function
;; name as a prefix (can pass in variable for function)
(defun satch--define-transient-function
    (function &optional hooks advise-symbols check)
  "Return a modified FUNCTION that removes itself from hooks/as advice.
The new function will automatically remove itself from HOOKS or
ADVISE-SYMBOLS after the first time it is called.

If CHECK is a function, only remove FUNCTION if calling CHECK returns non-nil

Though this can be used to create a transient function to add to both hooks and
as advice, there are some caveats:

- FUNCTION will be passed the hook or advice arguments; hooks often pass their
  functions no arguments but advice passes arguments; FUNCTION will need to be
  able to handle both cases if it is to be added to both a hook and as advice
- CHECK will also be passed the hook or advice arguments
- This will only work if you can add the same FUNCTION to all hooks or advice.
  If you need to make separate functions transient (e.g. because you need a
  different check per hook or advised function), a different method will be
  necessary. For an alternative method that can handle this case, see
  `once--make-functions-transient'."
  (let ((name (intern (format "satch--transient-%s%s%s"
                              ;; protect against collision
                              (satch--unique-count)
                              (if (symbolp function)
                                  (concat "-" (symbol-name function))
                                "")
                              (if (functionp check)
                                  (if (symbolp function)
                                      (format "-on-%s" check)
                                    (format "-on-lambda-check"))
                                "")))))
    (defalias name
      (lambda (&rest args)
        (when (or (not (functionp check))
                  (apply check args))
          (apply function args)
          (dolist (hook hooks)
            (remove-hook hook name))
          (dolist (symbol advise-symbols)
            (advice-remove symbol name))
          (fmakunbound name)))
      (format
       "Call a function with ARGS then remove it from hooks and/or as advice.
%sFunction to run: %s
Hooks: %s
Advised functions: %s"
       (if (functionp check)
           (format "Check to run/remove: %s\n" check)
         "")
       (if (symbolp function)
           (format "`%s'" function)
         (format "%s" function))
       hooks
       advise-symbols))
    name))

;;;###autoload
(cl-defun satch-add-hook (hooks functions &optional depth local &key transient)
  "A drop-in replacement for `add-hook'.
Unlike `add-hook', HOOKS and FUNCTIONS can be single items or lists.  DEPTH and
LOCAL are passed directly to `add-hook'.

Since this can add to multiple hooks, make sure not to mix HOOKS that run with
different numbers of arguments (or make sure that your specified FUNCTIONS
handle this).  Most hooks do not pass arguments, so FUNCTIONS will usually not
take any arguments, but keep this possibility in mind.

When TRANSIENT is non-nil, each function will remove itself from every hook in
HOOKS after it is run once.  If TRANSIENT is a function, call it when a hook
runs with any arguments to determine whether to continue.  If it returns nil, do
nothing.  If it returns non-nil, run the function and remove it from HOOKS.

In the future, this will automatically record hook additions using annalist.el."
  (satch--ensure-lists hooks functions)
  (dolist (fun functions)
    (when transient
      (setq fun (satch--define-transient-function fun hooks nil transient)))
    (dolist (hook hooks)
      (add-hook hook fun depth local))))

;;;###autoload
(defun satch-remove-hook (hooks functions &optional local)
  "A drop-in replacement for `remove-hook'.
Unlike `remove-hook', HOOKS and FUNCTIONS can be single items or lists.  LOCAL
is passed directly to `remove-hook'."
  (satch--ensure-lists hooks functions)
  (dolist (hook hooks)
    (dolist (fun functions)
      (remove-hook hook fun local))))

;; ** Advice Utilities
;;;###autoload
(cl-defun satch-advice-add (symbols
                            where
                            functions
                            &optional props
                            &key transient)
  "A drop-in replacement for `advice-add'.
SYMBOLS, WHERE, FUNCTIONS, and PROPS correspond to the arguments for
`advice-add'.  Unlike `advice-add', SYMBOLS and FUNCTIONS can be single items or
lists.

Usually you will specify multiple SYMBOLS and not multiple FUNCTIONS.  Note That
all FUNCTIONS must be able to handle the argument lists of all SYMBOLS.  Most of
the time when you specify either as a list, the function(s) will ignore all
arguments rather than try to handle different argument lists.

When TRANSIENT is non-nil, each function will remove itself as advice after it
is run once.  If TRANSIENT is a function, call it with the same arguments that
would be passed to the advice function.  If it returns nil, do nothing.  If it
returns non-nil, run the function and remove it from HOOKS.

In the future, this will automatically record advice using annalist.el."
  (satch--ensure-lists symbols functions)
  (dolist (function functions)
    (when transient
      (setq function (satch--define-transient-function function nil symbols
                                                       transient)))
    (dolist (sym symbols)
      (advice-add sym where function props))))

;; specify full autoload to prevent function indirection (autoload generation
;; will put a /flipped/ defalias into the autoloads file causing an infinite
;; loop)
;;;###autoload (autoload 'satch-add-advice "satch")
(defalias 'satch-add-advice #'satch-advice-add)

;;;###autoload
(defun satch-advice-remove (symbols functions)
  "A drop-in replacement for `advice-remove'.
Unlike `advice-remove', SYMBOLS and FUNCTIONS can be single items or lists."
  (satch--ensure-lists symbols functions)
  (dolist (symbol symbols)
    (dolist (func functions)
      (advice-remove symbol func))))

;;;###autoload (autoload 'satch-remove-advice "satch")
(defalias 'satch-remove-advice #'satch-advice-remove)

;; * Function Definition Utilities
;;;###autoload
(defmacro satch-defun (name arglist &optional docstring &rest body)
  "Define NAME as a function, returning the function.
This is `defun' but it is guaranteed to return the created function (`defun'
technically has an undefined return value)."
  (declare (doc-string 3) (indent 2))
  `(progn
     (defun ,name ,arglist ,docstring ,@body)
     #',name))

;;;###autoload
(defmacro satch-disable (mode)
  "Return a named function that disables MODE."
  (let ((name (intern (format "satch-disable-%s" mode))))
    `(satch-defun ,name (&rest _)
                  ,(format "Disable %s." mode)
                  (,mode -1))))

(provide 'satch)
;;; satch.el ends here